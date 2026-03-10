-module(af_lsp).

%% LSP transport layer: stdio JSON-RPC 2.0 framing.
%% Reads Content-Length headers from stdin, decodes JSON with jsx,
%% dispatches to the A4 LSP core, encodes responses back to JSON.

-export([main/1, start/0]).
-export([analyze_document/4, infer_stacks/1]).
-export([compute_hover/3, compute_completions/3, compute_definition/4]).
-export([format_stack/1, handle_request/2]).

-include("token.hrl").
-include("operation.hrl").

%% Escript entry point
main(_Args) ->
    start().

%% Start the LSP server
start() ->
    application:ensure_all_started(jsx),
    af_repl:init_types(),
    %% Compile the LSP A4 modules
    LspMod = compile_lsp_modules(),
    %% Initialize state
    State = init_lsp_state(LspMod),
    %% Enter the main loop
    loop(State).

%% Compile all LSP A4 files into BEAM modules
compile_lsp_modules() ->
    Files = [
        "src/bootstrap/lsp/lsp_types.a4",
        "src/bootstrap/lsp/lsp_stack_infer.a4",
        "src/bootstrap/lsp/lsp_server.a4"
    ],
    Mods = lists:foldl(fun(File, Acc) ->
        case af_ring2:compile_file_selfhosted(File, filename:rootname(filename:basename(File))) of
            {ok, Mod} -> [{filename:rootname(filename:basename(File)), Mod} | Acc];
            {error, Reason} ->
                log("Failed to compile ~s: ~p~n", [File, Reason]),
                Acc
        end
    end, [], Files),
    maps:from_list(Mods).

init_lsp_state(Mods) ->
    #{mods => Mods, docs => #{}, initialized => false}.

%% Main loop: read JSON-RPC message, dispatch, write response
loop(State) ->
    case read_message() of
        {ok, Request} ->
            {Response, NewState} = handle_request(Request, State),
            case Response of
                no_response -> ok;
                _ -> write_message(Response)
            end,
            case maps:get(<<"method">>, Request, undefined) of
                <<"exit">> -> halt(0);
                _ -> loop(NewState)
            end;
        eof ->
            ok;
        {error, Reason} ->
            log("Read error: ~p~n", [Reason]),
            ok
    end.

%% Read a JSON-RPC message from stdin
read_message() ->
    case read_headers() of
        {ok, ContentLength} ->
            case io:get_chars(standard_io, "", ContentLength) of
                eof -> eof;
                {error, _} = E -> E;
                Body ->
                    try jsx:decode(iolist_to_binary(Body), [return_maps])
                    catch _:_ -> {error, json_decode}
                    end
            end;
        eof -> eof;
        {error, _} = E -> E
    end.

%% Read Content-Length headers
read_headers() ->
    read_headers(undefined).

read_headers(ContentLength) ->
    case io:get_line(standard_io, "") of
        eof -> eof;
        {error, _} = E -> E;
        Line ->
            Trimmed = string:trim(Line),
            case Trimmed of
                "" when ContentLength =/= undefined ->
                    {ok, ContentLength};
                "" ->
                    read_headers(ContentLength);
                _ ->
                    case parse_header(Trimmed) of
                        {content_length, N} ->
                            read_headers(N);
                        _ ->
                            read_headers(ContentLength)
                    end
            end
    end.

parse_header(Line) ->
    case string:lowercase(Line) of
        "content-length: " ++ Rest ->
            {content_length, list_to_integer(string:trim(Rest))};
        _ ->
            case re:run(Line, "^[Cc]ontent-[Ll]ength:\\s*(\\d+)", [{capture, [1], list}]) of
                {match, [N]} -> {content_length, list_to_integer(N)};
                nomatch -> unknown
            end
    end.

%% Write a JSON-RPC response to stdout
write_message(Response) ->
    Body = jsx:encode(Response),
    Header = io_lib:format("Content-Length: ~B\r\n\r\n", [byte_size(Body)]),
    io:put_chars(standard_io, [Header, Body]).

%% Handle a JSON-RPC request
handle_request(#{<<"method">> := Method} = Request, State) ->
    Id = maps:get(<<"id">>, Request, undefined),
    Params = maps:get(<<"params">>, Request, #{}),
    case handle_method(Method, Id, Params, State) of
        {reply, Result, NewState} ->
            Response = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"result">> => Result
            },
            {Response, NewState};
        {notify, Notification, NewState} ->
            {Notification, NewState};
        {noreply, NewState} ->
            {no_response, NewState};
        {error_reply, Code, Msg, NewState} ->
            Response = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{<<"code">> => Code, <<"message">> => Msg}
            },
            {Response, NewState}
    end;
handle_request(_BadRequest, State) ->
    {no_response, State}.

%% Method dispatch
handle_method(<<"initialize">>, _Id, Params, State) ->
    RootUri = maps:get(<<"rootUri">>, Params, null),
    Capabilities = #{
        <<"textDocumentSync">> => 1,  %% Full sync
        <<"hoverProvider">> => true,
        <<"completionProvider">> => #{
            <<"triggerCharacters">> => [<<" ">>]
        },
        <<"definitionProvider">> => true
    },
    Result = #{
        <<"capabilities">> => Capabilities,
        <<"serverInfo">> => #{
            <<"name">> => <<"ActorForth LSP">>,
            <<"version">> => <<"0.1.0">>
        }
    },
    {reply, Result, State#{initialized => true, root_uri => RootUri}};

handle_method(<<"initialized">>, _Id, _Params, State) ->
    {noreply, State};

handle_method(<<"shutdown">>, _Id, _Params, State) ->
    {reply, null, State};

handle_method(<<"exit">>, _Id, _Params, State) ->
    {noreply, State};

handle_method(<<"textDocument/didOpen">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{
        <<"uri">> := Uri,
        <<"text">> := Text,
        <<"version">> := Version
    }} = Params,
    Doc = analyze_document(Uri, Text, Version, State),
    Docs = maps:get(docs, State, #{}),
    NewState = State#{docs => Docs#{Uri => Doc}},
    %% Publish diagnostics
    Diags = maps:get(diagnostics, Doc, []),
    Notification = publish_diagnostics(Uri, Diags),
    {notify, Notification, NewState};

handle_method(<<"textDocument/didChange">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri, <<"version">> := Version},
      <<"contentChanges">> := [#{<<"text">> := Text} | _]
    } = Params,
    Doc = analyze_document(Uri, Text, Version, State),
    Docs = maps:get(docs, State, #{}),
    NewState = State#{docs => Docs#{Uri => Doc}},
    Diags = maps:get(diagnostics, Doc, []),
    Notification = publish_diagnostics(Uri, Diags),
    {notify, Notification, NewState};

handle_method(<<"textDocument/didClose">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    Docs = maps:get(docs, State, #{}),
    {noreply, State#{docs => maps:remove(Uri, Docs)}};

handle_method(<<"textDocument/didSave">>, _Id, _Params, State) ->
    {noreply, State};

handle_method(<<"textDocument/hover">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri},
      <<"position">> := #{<<"line">> := Line, <<"character">> := Char}
    } = Params,
    case maps:get(Uri, maps:get(docs, State, #{}), undefined) of
        undefined ->
            {reply, null, State};
        Doc ->
            HoverResult = compute_hover(Doc, Line, Char),
            {reply, HoverResult, State}
    end;

handle_method(<<"textDocument/completion">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri},
      <<"position">> := #{<<"line">> := Line, <<"character">> := Char}
    } = Params,
    case maps:get(Uri, maps:get(docs, State, #{}), undefined) of
        undefined ->
            {reply, #{<<"isIncomplete">> => false, <<"items">> => []}, State};
        Doc ->
            Items = compute_completions(Doc, Line, Char),
            {reply, #{<<"isIncomplete">> => false, <<"items">> => Items}, State}
    end;

handle_method(<<"textDocument/definition">>, _Id, Params, State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri},
      <<"position">> := #{<<"line">> := Line, <<"character">> := Char}
    } = Params,
    case maps:get(Uri, maps:get(docs, State, #{}), undefined) of
        undefined ->
            {reply, null, State};
        Doc ->
            DefResult = compute_definition(Doc, Uri, Line, Char),
            {reply, DefResult, State}
    end;

handle_method(_Method, _Id, _Params, State) ->
    {noreply, State}.

%% Analyze a document: tokenize and infer stacks
analyze_document(Uri, Text, Version, _State) ->
    Tokens = try
        af_parser:parse(binary_to_list(Text), binary_to_list(Uri))
    catch _:_ -> []
    end,
    StackSnaps = infer_stacks(Tokens),
    Diagnostics = compute_diagnostics(Tokens, StackSnaps),
    #{uri => Uri, version => Version, text => Text,
      tokens => Tokens, stack_snaps => StackSnaps,
      diagnostics => Diagnostics}.

%% Stack inference: walk tokens, simulate stack effects
infer_stacks(Tokens) ->
    infer_stacks(Tokens, [], [], 0).

infer_stacks([], _Stack, Snaps, _Idx) ->
    lists:reverse(Snaps);
infer_stacks([Token | Rest], Stack, Snaps, Idx) ->
    Line = Token#token.line,
    Col = Token#token.column,
    Value = Token#token.value,
    Snap = #{idx => Idx, line => Line, col => Col,
             token => Value, stack => Stack},
    NewStack = apply_stack_effect(Value, Token, Stack),
    infer_stacks(Rest, NewStack, [Snap | Snaps], Idx + 1).

%% Apply stack effect of a token
apply_stack_effect(":", _Token, Stack) ->
    [{def_marker} | Stack];
apply_stack_effect(";", _Token, Stack) ->
    Stack;
apply_stack_effect(".", _Token, Stack) ->
    %% End of definition — pop back to def_marker or leave as-is
    drop_to_marker(Stack);
apply_stack_effect("->", _Token, Stack) ->
    Stack;

%% Stack primitives
apply_stack_effect("dup", _Token, [H | T]) -> [H, H | T];
apply_stack_effect("drop", _Token, [_ | T]) -> T;
apply_stack_effect("swap", _Token, [A, B | T]) -> [B, A | T];
apply_stack_effect("rot", _Token, [A, B, C | T]) -> [C, A, B | T];
apply_stack_effect("over", _Token, [A, B | T]) -> [B, A, B | T];
apply_stack_effect("2dup", _Token, [A, B | T]) -> [A, B, A, B | T];

%% Arithmetic: two numbers -> one number
apply_stack_effect("+", _Token, [_, _ | T]) -> ["Int" | T];
apply_stack_effect("-", _Token, [_, _ | T]) -> ["Int" | T];
apply_stack_effect("*", _Token, [_, _ | T]) -> ["Int" | T];
apply_stack_effect("/", _Token, [_, _ | T]) -> ["Int" | T];
apply_stack_effect("mod", _Token, [_, _ | T]) -> ["Int" | T];

%% Comparison: two values -> Bool
apply_stack_effect("==", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect("!=", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect("<", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect(">", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect("<=", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect(">=", _Token, [_, _ | T]) -> ["Bool" | T];

%% Logic
apply_stack_effect("not", _Token, [_ | T]) -> ["Bool" | T];
apply_stack_effect("and", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect("or", _Token, [_, _ | T]) -> ["Bool" | T];

%% List operations
apply_stack_effect("nil", _Token, Stack) -> ["List" | Stack];
apply_stack_effect("cons", _Token, [_, _ | T]) -> ["List" | T];
apply_stack_effect("head", _Token, [_ | T]) -> ["Any" | T];
apply_stack_effect("tail", _Token, [_ | T]) -> ["List" | T];
apply_stack_effect("length", _Token, [_ | T]) -> ["Int" | T];
apply_stack_effect("empty?", _Token, [_ | T]) -> ["Bool" | T];
apply_stack_effect("append", _Token, [_, _ | T]) -> ["List" | T];
apply_stack_effect("reverse", _Token, [_ | T]) -> ["List" | T];

%% Map operations
apply_stack_effect("map-new", _Token, Stack) -> ["Map" | Stack];
apply_stack_effect("map-put", _Token, [_, _, _ | T]) -> ["Map" | T];
apply_stack_effect("map-get", _Token, [_, _ | T]) -> ["Any" | T];
apply_stack_effect("map-delete", _Token, [_, _ | T]) -> ["Map" | T];
apply_stack_effect("map-has?", _Token, [_, _ | T]) -> ["Bool" | T];
apply_stack_effect("map-keys", _Token, [_ | T]) -> ["List" | T];
apply_stack_effect("map-values", _Token, [_ | T]) -> ["List" | T];
apply_stack_effect("map-size", _Token, [_ | T]) -> ["Int" | T];

%% String operations
apply_stack_effect("concat", _Token, [_, _ | T]) -> ["String" | T];
apply_stack_effect("to-string", _Token, [_ | T]) -> ["String" | T];
apply_stack_effect("to-int", _Token, [_ | T]) -> ["Int" | T];
apply_stack_effect("to-float", _Token, [_ | T]) -> ["Float" | T];

%% IO
apply_stack_effect("print", _Token, [_ | T]) -> T;
apply_stack_effect("stack", _Token, Stack) -> Stack;
apply_stack_effect("assert", _Token, [_ | T]) -> T;
apply_stack_effect("assert-eq", _Token, [_, _ | T]) -> T;

%% Literals: integers push Int
apply_stack_effect(Value, Token, Stack) ->
    case Token#token.quoted of
        true -> ["String" | Stack];
        false ->
            case is_integer_literal(Value) of
                true -> ["Int" | Stack];
                false ->
                    case is_float_literal(Value) of
                        true -> ["Float" | Stack];
                        false ->
                            %% Try to look up as a known word
                            case lookup_word_effect(Value) of
                                {ok, Effect} -> apply_word_effect(Effect, Stack);
                                not_found -> ["Atom" | Stack]
                            end
                    end
            end
    end.

is_integer_literal(Value) ->
    case catch list_to_integer(Value) of
        N when is_integer(N) -> true;
        _ -> false
    end.

is_float_literal(Value) ->
    case catch list_to_float(Value) of
        F when is_float(F) -> true;
        _ -> false
    end.

%% Look up a word's stack effect from the type registry
lookup_word_effect(Name) ->
    %% Try to find in Any type first, then try as type constructor
    case af_type:find_op_by_name(list_to_atom(Name), 'Any') of
        {ok, Op} -> {ok, Op};
        not_found ->
            %% Check all types
            AllTypes = af_type:all_types(),
            find_in_types(list_to_atom(Name), AllTypes)
    end.

find_in_types(_Name, []) -> not_found;
find_in_types(Name, [Type | Rest]) ->
    TypeName = element(2, Type),  %% #af_type.name
    case af_type:find_op_by_name(Name, TypeName) of
        {ok, Op} -> {ok, Op};
        not_found -> find_in_types(Name, Rest)
    end.

apply_word_effect(Op, Stack) ->
    SigIn = Op#operation.sig_in,
    SigOut = Op#operation.sig_out,
    InLen = length(SigIn),
    case length(Stack) >= InLen of
        true ->
            Remaining = lists:nthtail(InLen, Stack),
            OutTypes = [atom_to_list(T) || T <- SigOut],
            OutTypes ++ Remaining;
        false ->
            %% Stack underflow — mark as unknown
            ["?" | Stack]
    end.

drop_to_marker([]) -> [];
drop_to_marker([{def_marker} | T]) -> T;
drop_to_marker([_ | T]) -> drop_to_marker(T).

%% Compute hover: find stack snapshot nearest to position
compute_hover(Doc, Line, Char) ->
    Snaps = maps:get(stack_snaps, Doc, []),
    case find_snap_at(Snaps, Line, Char) of
        undefined -> null;
        Snap ->
            Stack = maps:get(stack, Snap),
            Token = maps:get(token, Snap),
            StackStr = format_stack(Stack),
            Content = iolist_to_binary([
                <<"**">>, list_to_binary(Token), <<"**\n\n">>,
                <<"Stack: `">>, StackStr, <<"`\n\n">>,
                <<"```\n">>,
                format_stack_picture(Stack),
                <<"```">>
            ]),
            #{<<"contents">> => #{
                <<"kind">> => <<"markdown">>,
                <<"value">> => Content
            }}
    end.

find_snap_at([], _Line, _Char) -> undefined;
find_snap_at(Snaps, Line, Char) ->
    %% LSP lines are 0-indexed, our tokens are 1-indexed
    TargetLine = Line + 1,
    TargetCol = Char + 1,
    Matching = [S || S <- Snaps,
                     maps:get(line, S) =:= TargetLine,
                     maps:get(col, S) =< TargetCol],
    case Matching of
        [] ->
            %% Try the line before
            Before = [S || S <- Snaps, maps:get(line, S) =< TargetLine],
            case Before of
                [] -> undefined;
                _ -> lists:last(Before)
            end;
        _ -> lists:last(Matching)
    end.

format_stack([]) -> <<"( empty )">>;
format_stack(Stack) ->
    Items = lists:reverse(Stack),
    Strs = [list_to_binary(S) || S <- Items, is_list(S)],
    iolist_to_binary([<<"( ">>, lists:join(<<" ">>, Strs), <<" -- TOS )">>]).

format_stack_picture([]) -> <<"  (empty stack)\n">>;
format_stack_picture(Stack) ->
    Indexed = lists:zip(lists:seq(0, length(Stack) - 1), Stack),
    Lines = [io_lib:format("  ~s~s~n", [
        case I of 0 -> "TOS -> "; _ -> "       " end,
        S
    ]) || {I, S} <- Indexed, is_list(S)],
    iolist_to_binary(Lines).

%% Compute completions based on TOS type
compute_completions(Doc, Line, Char) ->
    Snaps = maps:get(stack_snaps, Doc, []),
    TosType = case find_snap_at(Snaps, Line, Char) of
        undefined -> 'Any';
        Snap ->
            Stack = maps:get(stack, Snap),
            case Stack of
                [Top | _] when is_list(Top) -> list_to_atom(Top);
                _ -> 'Any'
            end
    end,
    %% Get ops from TOS type + Any
    TosOps = get_type_ops(TosType),
    AnyOps = case TosType of
        'Any' -> [];
        _ -> get_type_ops('Any')
    end,
    AllOps = TosOps ++ AnyOps,
    [format_completion_item(Op) || Op <- AllOps].

get_type_ops(TypeName) ->
    case af_type:get_type(TypeName) of
        {ok, Type} ->
            Ops = element(3, Type),  %% #af_type.ops
            maps:fold(fun(Name, OpList, Acc) ->
                [{Name, Op} || Op <- OpList] ++ Acc
            end, [], Ops);
        not_found -> []
    end.

format_completion_item({Name, Op}) ->
    SigIn = Op#operation.sig_in,
    SigOut = Op#operation.sig_out,
    Detail = iolist_to_binary([
        format_sig(SigIn), <<" -> ">>, format_sig(SigOut)
    ]),
    #{
        <<"label">> => to_binary(Name),
        <<"detail">> => Detail,
        <<"kind">> => 3  %% Function
    }.

format_sig([]) -> <<"()">>;
format_sig(Types) ->
    Strs = [to_binary(T) || T <- Types],
    iolist_to_binary(lists:join(<<" ">>, Strs)).

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(B) when is_binary(B) -> B;
to_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).

%% Compute diagnostics from stack inference
compute_diagnostics(_Tokens, _Snaps) ->
    %% TODO: detect stack underflows, type mismatches
    [].

%% Compute go-to-definition
compute_definition(Doc, Uri, Line, Char) ->
    Tokens = maps:get(tokens, Doc, []),
    case find_token_at(Tokens, Line + 1, Char + 1) of
        undefined -> null;
        Token ->
            WordName = Token#token.value,
            case find_word_def_token(Tokens, WordName) of
                undefined -> null;
                DefToken ->
                    #{<<"uri">> => Uri,
                      <<"range">> => #{
                          <<"start">> => #{
                              <<"line">> => DefToken#token.line - 1,
                              <<"character">> => DefToken#token.column - 1
                          },
                          <<"end">> => #{
                              <<"line">> => DefToken#token.line - 1,
                              <<"character">> => DefToken#token.column - 1 + length(DefToken#token.value)
                          }
                      }
                    }
            end
    end.

find_token_at([], _Line, _Col) -> undefined;
find_token_at([T | Rest], Line, Col) ->
    TLine = T#token.line,
    TCol = T#token.column,
    TEnd = TCol + length(T#token.value),
    case TLine =:= Line andalso Col >= TCol andalso Col =< TEnd of
        true -> T;
        false -> find_token_at(Rest, Line, Col)
    end.

%% Find where a word is defined (token after ":")
find_word_def_token([], _Name) -> undefined;
find_word_def_token([#token{value = ":"} | [#token{value = Name} = DefTok | _]], Name) ->
    DefTok;
find_word_def_token([_ | Rest], Name) ->
    find_word_def_token(Rest, Name).

%% Publish diagnostics notification
publish_diagnostics(Uri, Diags) ->
    LspDiags = [#{
        <<"range">> => #{
            <<"start">> => #{<<"line">> => maps:get(line, D, 0) - 1,
                            <<"character">> => maps:get(col, D, 0) - 1},
            <<"end">> => #{<<"line">> => maps:get(line, D, 0) - 1,
                          <<"character">> => maps:get(end_col, D, 1) - 1}
        },
        <<"severity">> => maps:get(severity, D, 1),
        <<"source">> => <<"actorforth">>,
        <<"message">> => maps:get(message, D, <<"">>)
    } || D <- Diags],
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"method">> => <<"textDocument/publishDiagnostics">>,
      <<"params">> => #{
          <<"uri">> => Uri,
          <<"diagnostics">> => LspDiags
      }}.

log(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

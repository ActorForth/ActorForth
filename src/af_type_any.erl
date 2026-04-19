-module(af_type_any).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0, auto_compile_word/1]).

init() ->
    %% dup : _a -> _a _a
    af_type:add_op('Any', #operation{
        name = "dup", sig_in = ['_a'], sig_out = ['_a', '_a'],
        impl = fun op_dup/1
    }),

    %% drop : _ ->
    af_type:add_op('Any', #operation{
        name = "drop", sig_in = ['_'], sig_out = [],
        impl = fun op_drop/1
    }),

    %% pop : _ ->  (alias for drop)
    af_type:add_op('Any', #operation{
        name = "pop", sig_in = ['_'], sig_out = [],
        impl = fun op_drop/1
    }),

    %% . : Any ->  (print TOS, Forth-style)
    af_type:add_op('Any', #operation{
        name = ".", sig_in = ['Any'], sig_out = [],
        impl = fun op_dot/1
    }),

    %% .s : ->  (show stack, alias for stack)
    af_type:add_op('Any', #operation{
        name = ".s", sig_in = [], sig_out = [],
        impl = fun op_stack/1
    }),

    %% swap : _a _b -> _b _a
    af_type:add_op('Any', #operation{
        name = "swap", sig_in = ['_a', '_b'], sig_out = ['_b', '_a'],
        impl = fun op_swap/1
    }),

    %% rot : _a _b _c -> _c _a _b  (brings 3rd to top)
    af_type:add_op('Any', #operation{
        name = "rot", sig_in = ['_a', '_b', '_c'], sig_out = ['_c', '_a', '_b'],
        impl = fun op_rot/1
    }),

    %% over : _a _b -> _b _a _b  (copies 2nd to top)
    af_type:add_op('Any', #operation{
        name = "over", sig_in = ['_a', '_b'], sig_out = ['_b', '_a', '_b'],
        impl = fun op_over/1
    }),

    %% 2dup : _a _b -> _a _b _a _b
    af_type:add_op('Any', #operation{
        name = "2dup", sig_in = ['_a', '_b'], sig_out = ['_a', '_b', '_a', '_b'],
        impl = fun op_2dup/1
    }),

    %% print : Any ->
    af_type:add_op('Any', #operation{
        name = "print", sig_in = ['Any'], sig_out = [],
        impl = fun op_print/1
    }),

    %% stack : ->  (displays stack contents)
    af_type:add_op('Any', #operation{
        name = "stack", sig_in = [], sig_out = [],
        impl = fun op_stack/1
    }),

    %% words : ->  (displays available words)
    af_type:add_op('Any', #operation{
        name = "words", sig_in = [], sig_out = [],
        impl = fun op_words/1
    }),

    %% types : ->  (displays registered types)
    af_type:add_op('Any', #operation{
        name = "types", sig_in = [], sig_out = [],
        impl = fun op_types/1
    }),

    %% see : Atom ->  (display source/definition of a word)
    af_type:add_op('Any', #operation{
        name = "see", sig_in = ['Atom'], sig_out = [],
        impl = fun op_see/1
    }),

    %% assert : Bool ->  (passes silently if true, errors with location if false)
    af_type:add_op('Any', #operation{
        name = "assert", sig_in = ['Bool'], sig_out = [],
        impl = fun op_assert/1
    }),

    %% assert-eq : Any Any ->  (passes if equal type+value, errors with expected/actual)
    af_type:add_op('Any', #operation{
        name = "assert-eq", sig_in = ['Any', 'Any'], sig_out = [],
        impl = fun op_assert_eq/1
    }),

    %% load : String ->  (load and interpret a .a4 file)
    af_type:add_op('Any', #operation{
        name = "load", sig_in = ['String'], sig_out = [],
        impl = fun op_load/1
    }),

    %% import : String -> Atom  (compile .a4 file to BEAM module, load words)
    af_type:add_op('Any', #operation{
        name = "import", sig_in = ['String'], sig_out = ['Atom'],
        impl = fun op_import/1
    }),

    %% compile : String ->  (compile a word to native BEAM function)
    af_type:add_op('Any', #operation{
        name = "compile", sig_in = ['String'], sig_out = [],
        impl = fun op_compile/1
    }),

    %% auto-compile : Bool ->  (toggle auto-compilation of words to BEAM)
    af_type:add_op('Any', #operation{
        name = "auto-compile", sig_in = ['Bool'], sig_out = [],
        impl = fun op_auto_compile/1
    }),

    %% read-file : String -> String  (read entire file as string)
    af_type:add_op('Any', #operation{
        name = "read-file", sig_in = ['String'], sig_out = ['String'],
        impl = fun op_read_file/1
    }),

    %% write-file : String String ->  (content path --)
    af_type:add_op('Any', #operation{
        name = "write-file", sig_in = ['String', 'String'], sig_out = [],
        impl = fun op_write_file/1
    }),

    %% file-exists? : String -> Bool
    af_type:add_op('Any', #operation{
        name = "file-exists?", sig_in = ['String'], sig_out = ['Bool'],
        impl = fun op_file_exists/1
    }),

    %% get-word-defs : String -> List  (get compiled word definitions)
    af_type:add_op('Any', #operation{
        name = "get-word-defs", sig_in = ['String'], sig_out = ['List'],
        impl = fun op_get_word_defs/1
    }),

    %% get-all-words : -> List  (get all user-defined word names)
    af_type:add_op('Any', #operation{
        name = "get-all-words", sig_in = [], sig_out = ['List'],
        impl = fun op_get_all_words/1
    }),

    %% tokenize : String String -> List  (source filename -> tokens)
    af_type:add_op('Any', #operation{
        name = "tokenize", sig_in = ['String', 'String'], sig_out = ['List'],
        impl = fun op_tokenize/1
    }),

    %% debug : -> Debug  (pushes Debug marker, handler intercepts on/off)
    af_type:register_type(#af_type{name = 'Debug'}),
    af_type:add_op('Any', #operation{
        name = "debug", sig_in = [], sig_out = ['Debug'],
        impl = fun op_debug/1
    }),
    af_type:register_type(#af_type{
        name = 'Debug',
        ops = get_ops('Debug'),
        handler = fun handle_debug/2
    }),

    %% forget : String ->  (remove named word plus everything defined after it)
    af_type:add_op('Any', #operation{
        name = "forget", sig_in = ['String'], sig_out = [],
        impl = fun op_forget/1
    }),

    %% pending : ->  (print the list of unresolved type checks)
    af_type:add_op('Any', #operation{
        name = "pending", sig_in = [], sig_out = [],
        impl = fun op_pending/1
    }),

    %% verify-all : ->  (force a final retry, print any unresolved)
    af_type:add_op('Any', #operation{
        name = "verify-all", sig_in = [], sig_out = [],
        impl = fun op_verify_all/1
    }),

    ok.

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%%% Operations

op_dup(Cont) ->
    [Top | _] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [Top | Cont#continuation.data_stack]}.

op_drop(Cont) ->
    [_ | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest}.

op_swap(Cont) ->
    [A, B | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [B, A | Rest]}.

op_rot(Cont) ->
    [A, B, C | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [C, A, B | Rest]}.

op_over(Cont) ->
    [A, B | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [B, A, B | Rest]}.

op_2dup(Cont) ->
    [A, B | _] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [A, B | Cont#continuation.data_stack]}.

op_print(Cont) ->
    [Item | Rest] = Cont#continuation.data_stack,
    Value = case Item of
        {_Type, V} -> V;
        Tuple when is_tuple(Tuple), tuple_size(Tuple) >= 2 ->
            %% Product type instance: show as {TypeName, V1, V2, ...}
            Tuple;
        Other -> Other
    end,
    io:format("~p~n", [Value]),
    Cont#continuation{data_stack = Rest}.

op_dot(Cont) ->
    [Item | Rest] = Cont#continuation.data_stack,
    case Item of
        {Type, Value} when is_atom(Type) ->
            io:format("~p : ~s~n", [Value, Type]);
        Tuple when is_tuple(Tuple), tuple_size(Tuple) >= 2 ->
            TypeName = element(1, Tuple),
            Fields = tl(tuple_to_list(Tuple)),
            io:format("~p : ~s~n", [Fields, TypeName])
    end,
    Cont#continuation{data_stack = Rest}.

op_stack(Cont) ->
    Stack = Cont#continuation.data_stack,
    case Stack of
        [] -> io:format("Stack empty~n");
        _ ->
            io:format("Stack(~p):~n", [length(Stack)]),
            lists:foldl(fun(Item, Idx) ->
                case Item of
                    {Type, Val} when is_atom(Type) ->
                        io:format("  ~p) ~p : ~p~n", [Idx, Val, Type]);
                    Tuple when is_tuple(Tuple), tuple_size(Tuple) >= 2 ->
                        %% Product type: {TypeName, V1, V2, ...}
                        TypeName = element(1, Tuple),
                        Fields = tl(tuple_to_list(Tuple)),
                        io:format("  ~p) ~p : ~p~n", [Idx, Fields, TypeName]);
                    Other ->
                        io:format("  ~p) ~p : ?~n", [Idx, Other])
                end,
                Idx + 1
            end, 0, Stack)
    end,
    Cont.

op_words(Cont) ->
    Types = af_type:all_types(),
    lists:foreach(fun(#af_type{name = Name, ops = Ops}) ->
        case maps:size(Ops) of
            0 -> ok;
            _ ->
                Words = lists:sort(maps:keys(Ops)),
                io:format("~p : ~p~n", [Name, Words])
        end
    end, Types),
    Cont.

op_types(Cont) ->
    Types = [T#af_type.name || T <- af_type:all_types()],
    io:format("Types: ~p~n", [lists:sort(Types)]),
    Cont.

%%% See

op_see(Cont) ->
    [{'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    AllTypes = af_type:all_types(),
    Matches = lists:flatmap(fun(#af_type{name = TypeName, ops = Ops}) ->
        case maps:get(WordName, Ops, []) of
            [] -> [];
            OpList -> [{TypeName, Op} || Op <- OpList]
        end
    end, AllTypes),
    case Matches of
        [] ->
            io:format("Word '~s' not found.~n", [WordName]);
        _ ->
            lists:foreach(fun({TypeName, Op}) ->
                print_word_definition(TypeName, Op)
            end, Matches)
    end,
    Cont#continuation{data_stack = Rest}.

print_word_definition(TypeName, #operation{name = Name, sig_in = SigIn, sig_out = SigOut, source = Source}) ->
    %% Format signature
    SigInStr = format_sig(SigIn),
    SigOutStr = format_sig(SigOut),
    io:format("  : ~s ~s -> ~s ;", [Name, SigInStr, SigOutStr]),
    case Source of
        {compiled, Body} ->
            BodyStr = string:join([Op#operation.name || Op <- Body], " "),
            io:format(" ~s .~n", [BodyStr]);
        {native, Mod} ->
            io:format("  [native: ~p]~n", [Mod]);
        auto ->
            io:format("  [auto-generated]~n");
        _ ->
            io:format("  [built-in]~n")
    end,
    io:format("    in type: ~p~n", [TypeName]).

format_sig([]) -> "";
format_sig(Sig) ->
    %% Sig is TOS-first; display in Forth order (deepest first)
    Reversed = lists:reverse(Sig),
    string:join([format_sig_item(S) || S <- Reversed], " ").

format_sig_item({Type, Value}) ->
    lists:flatten(io_lib:format("~p(~p)", [Type, Value]));
format_sig_item(Type) when is_atom(Type) ->
    atom_to_list(Type).

%%% Assert

op_assert(Cont) ->
    [{'Bool', Val} | Rest] = Cont#continuation.data_stack,
    case Val of
        true ->
            Cont#continuation{data_stack = Rest};
        false ->
            Msg = "Assertion failed: expected True on stack",
            af_error:raise(assertion_failed, Msg, Cont)
    end.

op_assert_eq(Cont) ->
    [Expected, Actual | Rest] = Cont#continuation.data_stack,
    case Expected =:= Actual of
        true ->
            Cont#continuation{data_stack = Rest};
        false ->
            Msg = lists:flatten(io_lib:format(
                "Expected ~s but got ~s",
                [af_error:format_value(Expected), af_error:format_value(Actual)]
            )),
            af_error:raise(assert_eq_failed, Msg, Cont)
    end.

%%% Load

op_load(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    %% Resolve relative paths against the current file's directory
    ResolvedPath = case filename:pathtype(Path) of
        absolute -> Path;
        _ ->
            CurrentFile = case Cont#continuation.current_token of
                #token{file = File} when File =/= "", File =/= "stdin", File =/= "eval" ->
                    filename:dirname(File);
                _ -> "."
            end,
            filename:join(CurrentFile, Path)
    end,
    case file:read_file(ResolvedPath) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), ResolvedPath),
            Cont1 = Cont#continuation{data_stack = Rest},
            af_interpreter:interpret_tokens(Tokens, Cont1);
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("Cannot load file ~s: ~p", [ResolvedPath, Reason])),
            af_error:raise(load_error, Msg, Cont)
    end.

%%% Import

op_import(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    %% Resolve relative paths
    ResolvedPath = case filename:pathtype(Path) of
        absolute -> Path;
        _ ->
            CurrentFile = case Cont#continuation.current_token of
                #token{file = File} when File =/= "", File =/= "stdin", File =/= "eval" ->
                    filename:dirname(File);
                _ -> "."
            end,
            filename:join(CurrentFile, Path)
    end,
    case af_compile_file:compile(ResolvedPath) of
        {ok, ModAtom} ->
            Cont#continuation{data_stack = [{'Atom', atom_to_list(ModAtom)} | Rest]};
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("import failed for ~s: ~p", [ResolvedPath, Reason])),
            af_error:raise(import_error, Msg, Cont)
    end.

%%% File I/O

op_read_file(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    case file:read_file(Path) of
        {ok, Content} ->
            Cont#continuation{data_stack = [{'String', Content} | Rest]};
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("Cannot read file ~s: ~p", [Path, Reason])),
            af_error:raise(file_error, Msg, Cont)
    end.

op_write_file(Cont) ->
    [{'String', PathBin}, {'String', Content} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    case file:write_file(Path, Content) of
        ok ->
            Cont#continuation{data_stack = Rest};
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("Cannot write file ~s: ~p", [Path, Reason])),
            af_error:raise(file_error, Msg, Cont)
    end.

op_file_exists(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    Exists = filelib:is_regular(Path),
    Cont#continuation{data_stack = [{'Bool', Exists} | Rest]}.

%%% Reflection / Introspection

op_get_word_defs(Cont) ->
    [{'String', NameBin} | Rest] = Cont#continuation.data_stack,
    Name = binary_to_list(NameBin),
    WordDefs = af_word_compiler:find_compiled_word_defs(Name),
    %% Convert to list of maps for A4 consumption
    DefList = lists:map(fun(Def) ->
        %% Def is either 4-tuple or 5-tuple (guarded). We expose only the
        %% first four fields; guard surfaces through other paths.
        WName = element(1, Def),
        SigIn = element(2, Def),
        SigOut = element(3, Def),
        Body = element(4, Def),
        {'Map', #{
            {'String', <<"name">>} => {'String', list_to_binary(WName)},
            {'String', <<"sig_in">>} => {'List', [sig_item_to_stack(S) || S <- SigIn]},
            {'String', <<"sig_out">>} => {'List', [sig_item_to_stack(S) || S <- SigOut]},
            {'String', <<"body">>} => {'List', body_to_stack(Body)}
        }}
    end, WordDefs),
    Cont#continuation{data_stack = [{'List', DefList} | Rest]}.

op_get_all_words(Cont) ->
    AllTypes = af_type:all_types(),
    AllNames = lists:usort(lists:flatmap(fun(#af_type{ops = Ops}) ->
        [{'String', list_to_binary(K)} || K <- maps:keys(Ops),
         lists:any(fun(#operation{source = S}) ->
             S =/= undefined andalso S =/= auto
         end, maps:get(K, Ops, []))]
    end, AllTypes)),
    Cont#continuation{data_stack = [{'List', AllNames} | Cont#continuation.data_stack]}.

op_tokenize(Cont) ->
    [{'String', FilenameBin}, {'String', SourceBin} | Rest] = Cont#continuation.data_stack,
    Source = binary_to_list(SourceBin),
    Filename = binary_to_list(FilenameBin),
    Tokens = af_parser:parse(Source, Filename),
    %% Convert #token{} records to A4 maps
    TokenList = lists:map(fun(#token{value = V, line = L, column = C, file = F, quoted = Q}) ->
        {'Map', #{
            {'String', <<"value">>} => {'String', list_to_binary(V)},
            {'String', <<"line">>} => {'Int', L},
            {'String', <<"col">>} => {'Int', C},
            {'String', <<"file">>} => {'String', list_to_binary(F)},
            {'String', <<"quoted">>} => {'Bool', Q =:= true}
        }}
    end, Tokens),
    Cont#continuation{data_stack = [{'List', TokenList} | Rest]}.

sig_item_to_stack({Type, Value}) when is_atom(Type) ->
    {'Map', #{
        {'String', <<"type">>} => {'String', atom_to_binary(Type, utf8)},
        {'String', <<"value">>} => af_term:to_stack_item(Value)
    }};
sig_item_to_stack(Type) when is_atom(Type) ->
    {'String', atom_to_binary(Type, utf8)}.

body_to_stack(Body) when is_list(Body) ->
    [{'String', list_to_binary(Op#operation.name)} || Op <- Body];
body_to_stack(_) -> [].

%%% Debug

op_debug(Cont) ->
    Cont#continuation{
        data_stack = [{'Debug', #{}} | Cont#continuation.data_stack]
    }.

handle_debug("on", Cont) ->
    [{'Debug', _} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest, debug = true};
handle_debug("off", Cont) ->
    [{'Debug', _} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest, debug = false};
handle_debug(Other, _Cont) ->
    error({debug_expected_on_off, Other}).

%%% Compile

op_compile(Cont) ->
    [{'String', NameBin} | Rest] = Cont#continuation.data_stack,
    Name = binary_to_list(NameBin),
    WordDefs = af_word_compiler:find_compiled_word_defs(Name),
    case WordDefs of
        [] ->
            Msg = lists:flatten(io_lib:format("No compiled word '~s' found", [Name])),
            af_error:raise(compile_error, Msg, Cont);
        _ ->
            ModAtom = list_to_atom("af_native_" ++ Name),
            case af_word_compiler:compile_words_to_module(ModAtom, WordDefs) of
                {ok, ModAtom} ->
                    FunAtom = list_to_atom(Name),
                    %% For multi-clause words, use the broadest (type-only) sig
                    %% for the wrapper so it matches any input of the right types.
                    ByType = group_defs_by_type(WordDefs),
                    lists:foreach(fun({TargetType, Defs}) ->
                        BroadSigIn = broadest_sig_in(Defs),
                        BroadDef = find_broadest_def(Defs),
                        BroadSigOut = element(3, BroadDef),
                        Wrapper = af_word_compiler:make_wrapper(ModAtom, FunAtom, BroadSigIn, BroadSigOut),
                        af_type:replace_ops(TargetType, Name, [Wrapper])
                    end, ByType),
                    %% Sync local dictionary if present
                    Dict = case Cont#continuation.dictionary of
                        undefined -> undefined;
                        D ->
                            lists:foldl(fun({TargetType, _}, DAcc) ->
                                case af_type:get_type(TargetType) of
                                    {ok, T} -> maps:put(TargetType, T, DAcc);
                                    _ -> DAcc
                                end
                            end, D, ByType)
                    end,
                    Cont#continuation{data_stack = Rest, dictionary = Dict, dispatch_cache = #{}};
                {error, Reason} ->
                    Msg = lists:flatten(io_lib:format("Compile failed for '~s': ~p", [Name, Reason])),
                    af_error:raise(compile_error, Msg, Cont)
            end
    end.

%% Get the broadest sig_in (strip value constraints to bare types).
%% Accepts both 4-tuple and 5-tuple def shapes.
broadest_sig_in(Defs) ->
    SigIn = element(2, find_broadest_def(Defs)),
    [case S of {T, _V} -> T; T -> T end || S <- SigIn].

%% Return a guard shared by the whole group if any def has one. If defs
%% have different guards we don't try to unify them — the wrapper matches
%% the whole name, so the first non-undefined guard we find "stands for"
%% the group. (In multi-clause cases the individual clauses still carry
%% their own guards as sub-clause metadata inside the compiled module.)
broadest_guard(Defs) ->
    GetGuard = fun(D) ->
        %% Guard is the 5th element if present.
        case tuple_size(D) of
            5 -> element(5, D);
            _ -> undefined
        end
    end,
    case [G || D <- Defs, (G = GetGuard(D)) =/= undefined, G =/= []] of
        [First | _] -> First;
        [] -> undefined
    end.

%% Find the def with the broadest (no value constraints) sig_in,
%% or fall back to the last def if all have constraints.
find_broadest_def(Defs) ->
    case lists:filter(fun(Def) ->
        SigIn = element(2, Def),
        not lists:any(fun(S) -> is_tuple(S) end, SigIn)
    end, Defs) of
        [Def | _] -> Def;
        [] -> lists:last(Defs)
    end.

group_defs_by_type(WordDefs) ->
    %% Accepts 4-tuple (legacy, no guard) or 5-tuple (with guard) defs.
    Groups = lists:foldl(fun(Def, Acc) ->
        SigIn = element(2, Def),
        TargetType = case SigIn of
            [{T, _} | _] -> T;
            [T | _] when is_atom(T) -> T;
            [] -> 'Any'
        end,
        Existing = maps:get(TargetType, Acc, []),
        maps:put(TargetType, Existing ++ [Def], Acc)
    end, #{}, WordDefs),
    maps:to_list(Groups).

op_auto_compile(Cont) ->
    [{'Bool', Flag} | Rest] = Cont#continuation.data_stack,
    persistent_term:put(af_auto_compile, Flag),
    Cont#continuation{data_stack = Rest}.

%% forget <name-as-string>: Forth-style dictionary rollback.
op_forget(Cont) ->
    [{'String', Name} | Rest] = Cont#continuation.data_stack,
    NameStr = case Name of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L
    end,
    case af_type:forget(NameStr) of
        ok -> ok;
        {error, not_found} ->
            io:format("forget: word '~s' is not defined~n", [NameStr])
    end,
    Cont#continuation{data_stack = Rest}.

%% pending: print the list of unresolved (deferred) type checks.
op_pending(Cont) ->
    case af_type_compiler:list_pending_checks() of
        [] ->
            io:format("No pending type checks.~n");
        Pending ->
            io:format("~p pending type check(s):~n", [length(Pending)]),
            lists:foreach(fun({Name, SigIn, SigOut}) ->
                io:format("  ~s  (~p -> ~p)~n", [Name, SigIn, SigOut])
            end, Pending)
    end,
    Cont.

%% verify-all: force a final retry of all deferred checks, then report.
op_verify_all(Cont) ->
    af_type_compiler:finalize_pending_checks(),
    Cont.

%% Called by af_type_compiler after word registration when auto-compile is enabled.
auto_compile_word(Name) ->
    case persistent_term:get(af_auto_compile, false) of
        false -> ok;
        true ->
            WordDefs = af_word_compiler:find_compiled_word_defs(Name),
            case WordDefs of
                [] -> ok;
                _ ->
                    ModAtom = list_to_atom("af_native_" ++ Name),
                    case af_word_compiler:compile_words_to_module(ModAtom, WordDefs) of
                        {ok, ModAtom} ->
                            FunAtom = list_to_atom(Name),
                            ByType = group_defs_by_type(WordDefs),
                            lists:foreach(fun({TargetType, Defs}) ->
                                BroadSigIn = broadest_sig_in(Defs),
                                BroadDef = find_broadest_def(Defs),
                                BroadSigOut = element(3, BroadDef),
                                %% If any def in this group has a guard, carry
                                %% it onto the wrapper so dispatch still checks
                                %% it at match time. The native function also
                                %% has the head guard compiled in; the double
                                %% check is cheap and keeps multi-clause fall-
                                %% through working in match_first_op.
                                BroadGuard = broadest_guard(Defs),
                                Wrapper0 = af_word_compiler:make_wrapper(
                                              ModAtom, FunAtom,
                                              BroadSigIn, BroadSigOut),
                                Wrapper = Wrapper0#operation{guard = BroadGuard},
                                af_type:replace_ops(TargetType, Name, [Wrapper])
                            end, ByType);
                        {error, _Reason} ->
                            ok
                    end
            end
    end.

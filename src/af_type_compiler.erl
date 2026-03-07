-module(af_type_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Compiler state stored on data_stack as tagged tuples:
%%   {'WordDefinition', #{name => ..., sig_in => ..., sig_out => ..., body => ...}}
%%   {'InputTypeSignature', Map}
%%   {'OutputTypeSignature', Map}
%%   {'CodeCompile', Map}
%%   {'SubClauseInput', Map}
%%   {'SubClauseOutput', Map}

init() ->
    %% : (colon) — starts a word definition. Registered in Any so it's always available.
    af_type:add_op('Any', #operation{
        name = ":",
        sig_in = [],
        sig_out = [],
        impl = fun op_colon/1
    }),

    %% Register the compiler state types
    af_type:register_type(#af_type{name = 'WordDefinition'}),
    af_type:register_type(#af_type{name = 'InputTypeSignature'}),
    af_type:register_type(#af_type{name = 'OutputTypeSignature'}),
    af_type:register_type(#af_type{name = 'CodeCompile'}),
    af_type:register_type(#af_type{name = 'SubClauseInput'}),
    af_type:register_type(#af_type{name = 'SubClauseOutput'}),

    %% -> in InputTypeSignature transitions to OutputTypeSignature
    af_type:add_op('InputTypeSignature', #operation{
        name = "->",
        sig_in = ['InputTypeSignature'],
        sig_out = ['OutputTypeSignature'],
        impl = fun op_arrow/1
    }),

    %% ; in OutputTypeSignature transitions to CodeCompile
    af_type:add_op('OutputTypeSignature', #operation{
        name = ";",
        sig_in = ['OutputTypeSignature'],
        sig_out = ['CodeCompile'],
        impl = fun op_semicolon/1
    }),

    %% . in CodeCompile finishes compilation
    af_type:add_op('CodeCompile', #operation{
        name = ".",
        sig_in = ['CodeCompile'],
        sig_out = [],
        impl = fun op_dot/1
    }),

    %% -> in SubClauseInput transitions to SubClauseOutput
    af_type:add_op('SubClauseInput', #operation{
        name = "->",
        sig_in = ['SubClauseInput'],
        sig_out = ['SubClauseOutput'],
        impl = fun op_sub_arrow/1
    }),

    %% ; in SubClauseOutput transitions back to CodeCompile
    af_type:add_op('SubClauseOutput', #operation{
        name = ";",
        sig_in = ['SubClauseOutput'],
        sig_out = ['CodeCompile'],
        impl = fun op_sub_semicolon/1
    }),

    %% Now set handlers on compiler types (must be after ops are registered)
    init_handlers(),

    ok.

%%% Compiler operations

%% : — Push WordDefinition state onto stack, changing the dictionary context.
op_colon(Cont) ->
    State = #{name => undefined, sig_in => [], sig_out => [], body => [],
              clauses => [], current_sub => undefined},
    Cont#continuation{
        data_stack = [{'WordDefinition', State} | Cont#continuation.data_stack]
    }.

%%% Type handlers — called by interpreter when no op matches in the type's dict.
init_handlers() ->
    af_type:register_type(#af_type{
        name = 'WordDefinition',
        ops = get_ops('WordDefinition'),
        handler = fun handle_word_definition/2
    }),
    af_type:register_type(#af_type{
        name = 'InputTypeSignature',
        ops = get_ops('InputTypeSignature'),
        handler = fun handle_input_sig/2
    }),
    af_type:register_type(#af_type{
        name = 'OutputTypeSignature',
        ops = get_ops('OutputTypeSignature'),
        handler = fun handle_output_sig/2
    }),
    af_type:register_type(#af_type{
        name = 'CodeCompile',
        ops = get_ops('CodeCompile'),
        handler = fun handle_code_compile/2
    }),
    af_type:register_type(#af_type{
        name = 'SubClauseInput',
        ops = get_ops('SubClauseInput'),
        handler = fun handle_sub_clause_input/2
    }),
    af_type:register_type(#af_type{
        name = 'SubClauseOutput',
        ops = get_ops('SubClauseOutput'),
        handler = fun handle_sub_clause_output/2
    }).

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%% Handler: WordDefinition
%% Capture token as the word name and transition to InputTypeSignature.
handle_word_definition(TokenValue, Cont) ->
    [{'WordDefinition', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{name => TokenValue},
    Cont#continuation{
        data_stack = [{'InputTypeSignature', NewState} | Rest]
    }.

%% Handler: InputTypeSignature
%% Accumulate type names into sig_in
handle_input_sig(TokenValue, Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    #{sig_in := SigIn} = State,
    TypeAtom = list_to_atom(TokenValue),
    NewState = State#{sig_in => SigIn ++ [TypeAtom]},
    Cont#continuation{
        data_stack = [{'InputTypeSignature', NewState} | Rest]
    }.

%% Handler: OutputTypeSignature
%% Accumulate type names into sig_out
handle_output_sig(TokenValue, Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    #{sig_out := SigOut} = State,
    TypeAtom = list_to_atom(TokenValue),
    NewState = State#{sig_out => SigOut ++ [TypeAtom]},
    Cont#continuation{
        data_stack = [{'OutputTypeSignature', NewState} | Rest]
    }.

%% Handler: CodeCompile
%% ":" starts a sub-clause; other tokens are compiled as body ops.
handle_code_compile(":", Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{body := Body, clauses := Clauses, current_sub := CurrentSub} = State,
    %% Complete any active sub-clause
    NewClauses = case CurrentSub of
        undefined -> Clauses;
        #{sub_sig_in := SSI, sub_sig_out := SSO} ->
            Clauses ++ [#{sig_in => SSI, sig_out => SSO, body => Body}]
    end,
    %% Transition to SubClauseInput
    SubState = State#{
        clauses => NewClauses,
        current_sub => undefined,
        body => [],
        sub_sig_in => [],
        sub_position => 0
    },
    Cont#continuation{
        data_stack = [{'SubClauseInput', SubState} | Rest]
    };
handle_code_compile(TokenValue, Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{body := Body} = State,
    %% Compile token as a late-binding thunk, preserving quoted flag
    OrigToken = Cont#continuation.current_token,
    Op = resolve_compile_token(TokenValue, OrigToken),
    NewState = State#{body => Body ++ [Op]},
    Cont#continuation{
        data_stack = [{'CodeCompile', NewState} | Rest]
    }.

%% Handler: SubClauseInput
%% Accumulate raw token info; resolution happens in op_sub_arrow after all tokens collected.
handle_sub_clause_input(TokenValue, Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := SubSigIn} = State,
    Token = Cont#continuation.current_token,
    IsQuoted = case Token of #token{quoted = true} -> true; _ -> false end,
    NewState = State#{
        sub_sig_in => SubSigIn ++ [{TokenValue, IsQuoted}]
    },
    Cont#continuation{
        data_stack = [{'SubClauseInput', NewState} | Rest]
    }.

%% Handler: SubClauseOutput
%% Accumulate raw token info; resolution happens in op_sub_semicolon.
handle_sub_clause_output(TokenValue, Cont) ->
    [{'SubClauseOutput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_out := SubSigOut} = State,
    Token = Cont#continuation.current_token,
    IsQuoted = case Token of #token{quoted = true} -> true; _ -> false end,
    NewState = State#{
        sub_sig_out => SubSigOut ++ [{TokenValue, IsQuoted}]
    },
    Cont#continuation{
        data_stack = [{'SubClauseOutput', NewState} | Rest]
    }.


%% Try to parse a token as a literal of a specific type using its literal handler.
try_literal_for_type(TokenValue, TypeName) ->
    case af_type:find_op_by_name("literal", TypeName) of
        {ok, #operation{impl = Impl}} ->
            TempCont = #continuation{data_stack = [{'Atom', TokenValue}]},
            try
                ResultCont = Impl(TempCont),
                [{T, V} | _] = ResultCont#continuation.data_stack,
                {ok, {T, V}}
            catch _:_ ->
                not_found
            end;
        not_found ->
            not_found
    end.

%% Compile a token as a late-binding thunk, preserving quoted flag.
resolve_compile_token(TokenValue, OrigToken) ->
    IsQuoted = case OrigToken of
        #token{quoted = Q} -> Q;
        _ -> false
    end,
    #operation{
        name = TokenValue,
        sig_in = [],
        sig_out = [],
        impl = fun(Cont) ->
            Token = #token{value = TokenValue, quoted = IsQuoted},
            af_interpreter:interpret_token(Token, Cont)
        end
    }.

%% -> transition: InputTypeSignature -> OutputTypeSignature
op_arrow(Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{
        data_stack = [{'OutputTypeSignature', State} | Rest]
    }.

%% ; transition: OutputTypeSignature -> CodeCompile
op_semicolon(Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{
        data_stack = [{'CodeCompile', State} | Rest]
    }.

%% -> in SubClauseInput: resolve accumulated tokens right-aligned, transition to SubClauseOutput
op_sub_arrow(Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := RawSubSigIn, sig_in := MasterSigIn} = State,
    %% Resolve raw tokens right-aligned against master sig
    ResolvedSubSigIn = resolve_sub_tokens_right_aligned(RawSubSigIn, MasterSigIn),
    NewState = State#{sub_sig_in => ResolvedSubSigIn, sub_sig_out => [], sub_out_position => 0},
    Cont#continuation{
        data_stack = [{'SubClauseOutput', NewState} | Rest]
    }.

%% ; in SubClauseOutput: resolve output tokens right-aligned, transition to CodeCompile
op_sub_semicolon(Cont) ->
    [{'SubClauseOutput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := SubSigIn, sub_sig_out := RawSubSigOut, sig_out := MasterSigOut} = State,
    ResolvedSubSigOut = resolve_sub_tokens_right_aligned(RawSubSigOut, MasterSigOut),
    NewState = State#{
        current_sub => #{sub_sig_in => SubSigIn, sub_sig_out => ResolvedSubSigOut},
        body => []
    },
    Cont#continuation{
        data_stack = [{'CodeCompile', NewState} | Rest]
    }.

%% . — Finish compilation: save the word to the appropriate type dictionary
op_dot(Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{clauses := Clauses, current_sub := CurrentSub, body := Body} = State,
    case Clauses =:= [] andalso CurrentSub =:= undefined of
        true ->
            %% Normal case: single operation, no sub-clauses
            register_single_word(State, Rest, Cont);
        false ->
            %% Multi-clause: complete any active sub-clause and register all
            AllClauses = case CurrentSub of
                undefined -> Clauses;
                #{sub_sig_in := SSI, sub_sig_out := SSO} ->
                    Clauses ++ [#{sig_in => SSI, sig_out => SSO, body => Body}]
            end,
            register_multi_word(State, AllClauses, Rest, Cont)
    end.

register_single_word(State, Rest, Cont) ->
    #{name := Name, sig_in := SigIn0, sig_out := SigOut0, body := Body} = State,

    %% Signatures are accumulated left-to-right (Forth convention: leftmost = deepest).
    %% Reverse so element 0 = TOS for match_sig and dispatch.
    SigIn = lists:reverse(SigIn0),
    SigOut = lists:reverse(SigOut0),

    %% Compile-time type check
    type_check_body(Name, SigIn, SigOut, Body),

    Impl = make_word_impl(Body, Name),

    %% Register in the TOS type's dict (first element after reversal).
    TargetType = get_target_type(SigIn),

    NewOp = #operation{
        name = Name,
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl,
        source = {compiled, Body}
    },

    ensure_type(TargetType),
    af_type:add_op(TargetType, NewOp),

    Cont#continuation{data_stack = Rest}.

register_multi_word(State, Clauses, Rest, Cont) ->
    #{name := Name, sig_in := MasterSigIn0} = State,
    MasterSigIn = lists:reverse(MasterSigIn0),
    TargetType = get_target_type(MasterSigIn),
    ensure_type(TargetType),

    %% Register each sub-clause as a separate operation.
    %% Value-constrained clauses come first (they were added in order),
    %% general clauses last — match_first_op tries in list order.
    lists:foreach(fun(#{sig_in := CSigIn0, sig_out := CSigOut0, body := CBody}) ->
        CSigIn = lists:reverse(CSigIn0),
        CSigOut = lists:reverse(CSigOut0),
        %% Type check each clause body
        type_check_body(Name, CSigIn, CSigOut, CBody),
        Impl = make_word_impl(CBody, Name),
        Op = #operation{
            name = Name,
            sig_in = CSigIn,
            sig_out = CSigOut,
            impl = Impl,
            source = {compiled, CBody}
        },
        af_type:add_op(TargetType, Op)
    end, Clauses),

    Cont#continuation{data_stack = Rest}.

%% Run compile-time type check on a word body.
%% Type mismatches are errors when inference is complete (no unknowns).
%% When inference encounters unknown ops (product types, etc.), downgrade to warning.
type_check_body(Name, SigIn, SigOut, Body) ->
    case af_type_check:check_word(Name, SigIn, SigOut, Body) of
        ok -> ok;
        {error, {type_mismatch, _, #{expected := Expected, actual := Actual}}} ->
            case has_unknown_types(Actual, Expected) of
                true ->
                    %% Inference was incomplete — warn only
                    io:format("Warning: type check for '~s' incomplete~n"
                              "  declared output: ~p~n"
                              "  inferred output: ~p~n", [Name, Expected, Actual]);
                false ->
                    %% Fully resolved types don't match — error
                    error({type_error, Name,
                        lists:flatten(io_lib:format(
                            "Type mismatch in word '~s': "
                            "declared output ~p but inferred ~p",
                            [Name, Expected, Actual]))})
            end;
        {error, {stack_underflow, OpName, _}} ->
            io:format("Warning: type check for '~s' incomplete "
                      "(stack underflow at '~p')~n", [Name, OpName]);
        {error, _Reason} ->
            %% Other inference errors — warn but allow
            ok
    end.

%% Check if inferred types suggest incomplete inference:
%% - Contains 'Atom' from unresolved tokens (product type ops, etc.)
%% - Different stack depth from expected (missing type info)
%% - Expected types include non-builtin types the checker can't verify
has_unknown_types(Types, Expected) when length(Types) =/= length(Expected) ->
    true;
has_unknown_types(Types, Expected) ->
    Builtins = ['Any', 'Int', 'Float', 'Bool', 'String', 'Atom', 'List', 'Map', 'Tuple'],
    lists:any(fun('Atom') -> true; (_) -> false end, Types)
    orelse lists:any(fun
        ({_T, _V}) -> false;  %% value constraints are checkable
        (T) -> not lists:member(T, Builtins)
    end, Expected).

get_target_type([{Type, _Value} | _]) -> Type;
get_target_type([Type | _]) when is_atom(Type) -> Type;
get_target_type([]) -> 'Any'.

ensure_type(TypeName) ->
    case af_type:get_type(TypeName) of
        not_found -> af_type:register_type(#af_type{name = TypeName});
        _ -> ok
    end.

%% Build an execution function from a compiled word body.
%% For tail-recursive words (last body token is a self-call), we pop the
%% trace frame BEFORE the tail call so the self-call is in Erlang tail
%% position, allowing the BEAM's native TCO to prevent stack growth.
make_word_impl(Body, WordName) ->
    case detect_tail_call(Body, WordName) of
        {true, InitBody, TailOp} ->
            fun(Cont) ->
                Cont1 = push_trace(WordName, Cont),
                Cont2 = execute_body(InitBody, Cont1),
                Cont3 = pop_trace(WordName, Cont2),
                (TailOp#operation.impl)(Cont3)
            end;
        false ->
            fun(Cont) ->
                Cont1 = push_trace(WordName, Cont),
                ResultCont = execute_body(Body, Cont1),
                pop_trace(WordName, ResultCont)
            end
    end.

push_trace(undefined, Cont) -> Cont;
push_trace(WordName, Cont) ->
    Frame = {WordName, Cont#continuation.current_token},
    Cont#continuation{word_trace = [Frame | Cont#continuation.word_trace]}.

pop_trace(undefined, Cont) -> Cont;
pop_trace(_WordName, Cont) ->
    Cont#continuation{word_trace = tl(Cont#continuation.word_trace)}.

%% Check if the last operation in Body is a call that can be tail-optimized.
%% Handles self-calls (self-recursion) and calls to other compiled words.
detect_tail_call([], _WordName) -> false;
detect_tail_call(_Body, undefined) -> false;
detect_tail_call(Body, WordName) ->
    Last = lists:last(Body),
    LastName = Last#operation.name,
    case LastName of
        WordName ->
            %% Self-recursive tail call
            {true, lists:droplast(Body), Last};
        _ ->
            %% Check if last op is a call to another compiled word
            case is_word_call(Last) of
                true -> {true, lists:droplast(Body), Last};
                false -> false
            end
    end.

%% A compiled word call has source = {compiled, _Body} indicating it's
%% a user-defined word (not a primitive), and thus benefits from TCO.
is_word_call(#operation{source = {compiled, _}}) -> true;
is_word_call(_) -> false.

execute_body([], Cont) -> Cont;
execute_body([#operation{impl = Impl} | Rest], Cont) ->
    NewCont = Impl(Cont),
    execute_body(Rest, NewCont).

%% Resolve accumulated raw sub-clause tokens right-aligned against master sig.
%% Raw tokens are {TokenValue, IsQuoted} pairs.
%% If sub has fewer tokens than master, pad left with master type atoms (wildcard match).
resolve_sub_tokens_right_aligned(RawTokens, MasterSig) ->
    SubLen = length(RawTokens),
    MasterLen = length(MasterSig),
    %% Right-align: tokens match rightmost positions of master sig
    Offset = MasterLen - SubLen,
    Resolved = lists:map(fun({Idx, {TokenValue, IsQuoted}}) ->
        MasterPos = Offset + Idx,
        case MasterPos >= 0 andalso MasterPos < MasterLen of
            true ->
                MasterType = lists:nth(MasterPos + 1, MasterSig),
                resolve_single_sub_token(TokenValue, IsQuoted, MasterType);
            false ->
                list_to_atom(TokenValue)
        end
    end, lists:zip(lists:seq(0, SubLen - 1), RawTokens)),
    %% Pad left with master types for unspecified positions
    case Offset > 0 of
        true ->
            Padding = lists:sublist(MasterSig, Offset),
            Padding ++ Resolved;
        false ->
            Resolved
    end.

%% Resolve a single sub-clause token against its master type.
resolve_single_sub_token(TokenValue, true, 'String') ->
    {'String', list_to_binary(TokenValue)};
resolve_single_sub_token(TokenValue, _IsQuoted, MasterType) ->
    case try_literal_for_type(TokenValue, MasterType) of
        {ok, {Type, Value}} -> {Type, Value};
        not_found -> list_to_atom(TokenValue)
    end.

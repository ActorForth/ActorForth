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
    %% Compile token as a late-binding thunk
    Op = resolve_compile_token(TokenValue),
    NewState = State#{body => Body ++ [Op]},
    Cont#continuation{
        data_stack = [{'CodeCompile', NewState} | Rest]
    }.

%% Handler: SubClauseInput
%% Resolve tokens as either value constraints or type names against master sig.
handle_sub_clause_input(TokenValue, Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    #{sig_in := MasterSigIn, sub_sig_in := SubSigIn, sub_position := Pos} = State,
    Resolved = resolve_sub_token(TokenValue, MasterSigIn, Pos),
    NewState = State#{
        sub_sig_in => SubSigIn ++ [Resolved],
        sub_position => Pos + 1
    },
    Cont#continuation{
        data_stack = [{'SubClauseInput', NewState} | Rest]
    }.

%% Handler: SubClauseOutput
%% Resolve tokens as either value constraints or type names against master sig.
handle_sub_clause_output(TokenValue, Cont) ->
    [{'SubClauseOutput', State} | Rest] = Cont#continuation.data_stack,
    #{sig_out := MasterSigOut, sub_sig_out := SubSigOut, sub_out_position := Pos} = State,
    Resolved = resolve_sub_token(TokenValue, MasterSigOut, Pos),
    NewState = State#{
        sub_sig_out => SubSigOut ++ [Resolved],
        sub_out_position => Pos + 1
    },
    Cont#continuation{
        data_stack = [{'SubClauseOutput', NewState} | Rest]
    }.

%% Resolve a sub-clause token against the master signature.
%% If the token can be parsed as a literal of the master type at this position,
%% it becomes a value constraint {Type, Value}. Otherwise it's a type name atom.
resolve_sub_token(TokenValue, MasterSig, Pos) ->
    case Pos < length(MasterSig) of
        true ->
            MasterType = lists:nth(Pos + 1, MasterSig),
            case try_literal_for_type(TokenValue, MasterType) of
                {ok, {Type, Value}} -> {Type, Value};
                not_found -> list_to_atom(TokenValue)
            end;
        false ->
            list_to_atom(TokenValue)
    end.

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

%% Compile a token as a late-binding thunk.
resolve_compile_token(TokenValue) ->
    #operation{
        name = TokenValue,
        sig_in = [],
        sig_out = [],
        impl = fun(Cont) ->
            Token = #token{value = TokenValue},
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

%% -> in SubClauseInput: freeze sub_sig_in, transition to SubClauseOutput
op_sub_arrow(Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{sub_sig_out => [], sub_out_position => 0},
    Cont#continuation{
        data_stack = [{'SubClauseOutput', NewState} | Rest]
    }.

%% ; in SubClauseOutput: transition back to CodeCompile with sub-clause context
op_sub_semicolon(Cont) ->
    [{'SubClauseOutput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := SubSigIn, sub_sig_out := SubSigOut} = State,
    NewState = State#{
        current_sub => #{sub_sig_in => SubSigIn, sub_sig_out => SubSigOut},
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

    Impl = make_word_impl(Body, Name),

    %% Register in the TOS type's dict (first element after reversal).
    TargetType = get_target_type(SigIn),

    NewOp = #operation{
        name = Name,
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl
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
        Impl = make_word_impl(CBody, Name),
        Op = #operation{
            name = Name,
            sig_in = CSigIn,
            sig_out = CSigOut,
            impl = Impl
        },
        af_type:add_op(TargetType, Op)
    end, Clauses),

    Cont#continuation{data_stack = Rest}.

get_target_type([{Type, _Value} | _]) -> Type;
get_target_type([Type | _]) when is_atom(Type) -> Type;
get_target_type([]) -> 'Any'.

ensure_type(TypeName) ->
    case af_type:get_type(TypeName) of
        not_found -> af_type:register_type(#af_type{name = TypeName});
        _ -> ok
    end.

%% Build an execution function from a compiled word body.
make_word_impl(Body, WordName) ->
    fun(Cont) ->
        Cont1 = case WordName of
            undefined -> Cont;
            _ ->
                Frame = {WordName, Cont#continuation.current_token},
                Cont#continuation{word_trace = [Frame | Cont#continuation.word_trace]}
        end,
        ResultCont = execute_body(Body, Cont1),
        case WordName of
            undefined -> ResultCont;
            _ -> ResultCont#continuation{word_trace = tl(ResultCont#continuation.word_trace)}
        end
    end.

execute_body([], Cont) -> Cont;
execute_body([#operation{impl = Impl} | Rest], Cont) ->
    NewCont = Impl(Cont),
    execute_body(Rest, NewCont).

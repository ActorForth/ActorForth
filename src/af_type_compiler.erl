-module(af_type_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).
-export([list_pending_checks/0, finalize_pending_checks/0,
         clear_pending_checks/0]).
-export([normalize_binding/2]).

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
              clauses => [], current_sub => undefined,
              pending_value => undefined},
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
%% Accumulate type names into sig_in.
%% Supports value constraints: "0 Int" -> {Int, 0}
%% When a literal (integer/float/bool) is seen, buffer it as pending_value.
%% When the next type name arrives, combine into {Type, Value}.
%%
%% Also supports guard expressions. The keyword `where` flips the handler into
%% guard-collecting mode; subsequent tokens up to `->` are accumulated as the
%% guard body. The guard is a sequence of words evaluated on a snapshot of the
%% stack at dispatch time; if the guard leaves {Bool, true} on top, the clause
%% matches.
handle_input_sig("where", Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{in_guard => true, guard => []},
    Cont#continuation{
        data_stack = [{'InputTypeSignature', NewState} | Rest]
    };
handle_input_sig(TokenValue, Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    case maps:get(in_guard, State, false) of
        true ->
            %% In guard-collecting mode: append the ORIGINAL token (which
            %% preserves quoted flag and position).
            Guard = maps:get(guard, State, []),
            Token = Cont#continuation.current_token,
            NewState = State#{guard => Guard ++ [Token]},
            Cont#continuation{
                data_stack = [{'InputTypeSignature', NewState} | Rest]
            };
        false ->
            handle_input_sig_type(TokenValue, State, Rest, Cont)
    end.

handle_input_sig_type(TokenValue, State, Rest, Cont) ->
    #{sig_in := SigIn, pending_value := PendingValue} = State,
    case PendingValue of
        undefined ->
            case try_parse_literal_value(TokenValue) of
                {ok, Value} ->
                    NewState = State#{pending_value => Value},
                    Cont#continuation{
                        data_stack = [{'InputTypeSignature', NewState} | Rest]
                    };
                not_found ->
                    TypeAtom = list_to_atom(TokenValue),
                    NewState = State#{sig_in => SigIn ++ [TypeAtom]},
                    Cont#continuation{
                        data_stack = [{'InputTypeSignature', NewState} | Rest]
                    }
            end;
        Value ->
            TypeAtom = list_to_atom(TokenValue),
            NewState = State#{sig_in => SigIn ++ [{TypeAtom, Value}],
                              pending_value => undefined},
            Cont#continuation{
                data_stack = [{'InputTypeSignature', NewState} | Rest]
            }
    end.

%% Handler: OutputTypeSignature
%% Accumulate type names into sig_out.
%% Supports value constraints same as input sig.
handle_output_sig(TokenValue, Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    #{sig_out := SigOut, pending_value := PendingValue} = State,
    case PendingValue of
        undefined ->
            case try_parse_literal_value(TokenValue) of
                {ok, Value} ->
                    NewState = State#{pending_value => Value},
                    Cont#continuation{
                        data_stack = [{'OutputTypeSignature', NewState} | Rest]
                    };
                not_found ->
                    TypeAtom = list_to_atom(TokenValue),
                    NewState = State#{sig_out => SigOut ++ [TypeAtom]},
                    Cont#continuation{
                        data_stack = [{'OutputTypeSignature', NewState} | Rest]
                    }
            end;
        Value ->
            TypeAtom = list_to_atom(TokenValue),
            NewState = State#{sig_out => SigOut ++ [{TypeAtom, Value}],
                              pending_value => undefined},
            Cont#continuation{
                data_stack = [{'OutputTypeSignature', NewState} | Rest]
            }
    end.

%% Handler: CodeCompile
%% ":" starts a sub-clause; other tokens are compiled as body ops.
%%
%% Pre-dispatch body preservation: if tokens accumulated into `body`
%% BEFORE the first `:` sub-clause marker, they are saved as
%% `pre_dispatch`. At `.` time this triggers single-op registration
%% with body = pre_dispatch ++ [select_clause]. Without this, the
%% classic `a b op : True -> ... : False -> ...` pattern would
%% silently lose `a b op`.
handle_code_compile(":", Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{body := Body, clauses := Clauses, current_sub := CurrentSub} = State,
    %% Complete any active sub-clause, carrying any guard through.
    NewClauses = case CurrentSub of
        undefined -> Clauses;
        #{sub_sig_in := SSI, sub_sig_out := SSO} = Sub ->
            Clauses ++ [#{sig_in => SSI,
                          sig_out => SSO,
                          body => Body,
                          guard => maps:get(sub_guard, Sub, undefined)}]
    end,
    %% Decide whether the accumulated Body is pre-dispatch code:
    %% non-empty AND this is the very first `:` (no existing clauses,
    %% no current_sub). Anything else means Body is a clause body.
    NewPreDispatch =
        case {CurrentSub, Clauses, Body} of
            {undefined, [], B} when B =/= [] ->
                B;
            _ ->
                maps:get(pre_dispatch, State, [])
        end,
    SubState = State#{
        clauses => NewClauses,
        current_sub => undefined,
        body => [],
        sub_sig_in => [],
        sub_position => 0,
        pre_dispatch => NewPreDispatch
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
%% Recognises the `where` keyword to switch into guard-collection mode; tokens
%% after `where` and before `->` are recorded as the sub-clause's guard body.
handle_sub_clause_input("where", Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{sub_in_guard => true, sub_guard => []},
    Cont#continuation{
        data_stack = [{'SubClauseInput', NewState} | Rest]
    };
handle_sub_clause_input(TokenValue, Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    case maps:get(sub_in_guard, State, false) of
        true ->
            Token = Cont#continuation.current_token,
            Guard = maps:get(sub_guard, State, []),
            NewState = State#{sub_guard => Guard ++ [Token]},
            Cont#continuation{
                data_stack = [{'SubClauseInput', NewState} | Rest]
            };
        false ->
            #{sub_sig_in := SubSigIn} = State,
            Token = Cont#continuation.current_token,
            IsQuoted = case Token of #token{quoted = true} -> true; _ -> false end,
            NewState = State#{
                sub_sig_in => SubSigIn ++ [{TokenValue, IsQuoted}]
            },
            Cont#continuation{
                data_stack = [{'SubClauseInput', NewState} | Rest]
            }
    end.

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


%% Try to parse a token value as a literal (integer, float, bool).
%% Used for value constraints in signatures: "0 Int" -> {Int, 0}
try_parse_literal_value(TokenValue) ->
    case catch list_to_integer(TokenValue) of
        N when is_integer(N) -> {ok, N};
        _ ->
            case catch list_to_float(TokenValue) of
                F when is_float(F) -> {ok, F};
                _ ->
                    case TokenValue of
                        "true" -> {ok, true};
                        "false" -> {ok, false};
                        _ -> not_found
                    end
            end
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

%% Compile a token as a late-binding thunk, preserving quoted flag.
%% Pre-build the #token{} record once at compile time and close over it
%% so each runtime call avoids rebuilding the record.
resolve_compile_token(TokenValue, OrigToken) ->
    IsQuoted = case OrigToken of
        #token{quoted = Q} -> Q;
        _ -> false
    end,
    Source = case IsQuoted of
        true -> quoted_string;
        false -> undefined
    end,
    PrebuiltToken = #token{value = TokenValue, quoted = IsQuoted},
    #operation{
        name = TokenValue,
        sig_in = [],
        sig_out = [],
        source = Source,
        impl = fun(Cont) ->
            af_interpreter:interpret_token(PrebuiltToken, Cont)
        end
    }.

%% -> transition: InputTypeSignature -> OutputTypeSignature
%% Also ends any guard-collecting mode so trailing tokens in the output sig
%% are parsed as types rather than guard words.
op_arrow(Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{in_guard => false},
    Cont#continuation{
        data_stack = [{'OutputTypeSignature', NewState} | Rest]
    }.

%% ; transition: OutputTypeSignature -> CodeCompile
op_semicolon(Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{
        data_stack = [{'CodeCompile', State} | Rest]
    }.

%% -> in SubClauseInput: resolve accumulated tokens right-aligned, transition to SubClauseOutput
%% Also ends any guard-collecting mode opened by `where`.
op_sub_arrow(Cont) ->
    [{'SubClauseInput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := RawSubSigIn, sig_in := MasterSigIn} = State,
    %% If this word has a pre-dispatch body, the sub-clause matches against
    %% the intermediate stack produced by that body — NOT the master sig.
    %% In that case, skip right-align padding (it would wrongly anchor the
    %% sig to master-sig positions) and resolve each token independently.
    HasPreDispatch = maps:get(pre_dispatch, State, []) =/= [],
    ResolvedSubSigIn = case HasPreDispatch of
        true -> resolve_sub_tokens_independent(RawSubSigIn);
        false -> resolve_sub_tokens_right_aligned(RawSubSigIn, MasterSigIn)
    end,
    NewState = State#{
        sub_sig_in => ResolvedSubSigIn,
        sub_sig_out => [],
        sub_out_position => 0,
        sub_in_guard => false
    },
    Cont#continuation{
        data_stack = [{'SubClauseOutput', NewState} | Rest]
    }.

%% ; in SubClauseOutput: resolve output tokens right-aligned, transition to CodeCompile
%% Carry the sub-clause guard (if any) into the clause record.
op_sub_semicolon(Cont) ->
    [{'SubClauseOutput', State} | Rest] = Cont#continuation.data_stack,
    #{sub_sig_in := SubSigIn, sub_sig_out := RawSubSigOut, sig_out := MasterSigOut} = State,
    ResolvedSubSigOut = resolve_sub_tokens_right_aligned(RawSubSigOut, MasterSigOut),
    SubGuard = maps:get(sub_guard, State, undefined),
    ClauseRec = #{sub_sig_in => SubSigIn,
                  sub_sig_out => ResolvedSubSigOut,
                  sub_guard => SubGuard},
    NewState = State#{
        current_sub => ClauseRec,
        body => [],
        sub_guard => undefined
    },
    Cont#continuation{
        data_stack = [{'CodeCompile', NewState} | Rest]
    }.

%% . — Finish compilation: save the word to the appropriate type dictionary
op_dot(Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{clauses := Clauses, current_sub := CurrentSub, body := Body} = State,
    PreDispatch = maps:get(pre_dispatch, State, []),
    case Clauses =:= [] andalso CurrentSub =:= undefined of
        true ->
            %% Normal case: single operation, no sub-clauses
            register_single_word(State, Rest, Cont);
        false ->
            %% Multi-clause: complete any active sub-clause and register all.
            %% Carry per-sub-clause guards through.
            AllClauses = case CurrentSub of
                undefined -> Clauses;
                #{sub_sig_in := SSI, sub_sig_out := SSO} = Sub ->
                    Clauses ++ [#{sig_in => SSI,
                                  sig_out => SSO,
                                  body => Body,
                                  guard => maps:get(sub_guard, Sub, undefined)}]
            end,
            case PreDispatch of
                [] ->
                    %% Plain multi-clause: each clause is its own op,
                    %% dispatched by match_sig at call time.
                    register_multi_word(State, AllClauses, Rest, Cont);
                _ ->
                    %% Pre-dispatch body + sub-clauses: build one op whose
                    %% body is the pre-dispatch tokens followed by a runtime
                    %% select-clause dispatcher.
                    register_predispatch_word(State, AllClauses,
                                              PreDispatch, Rest, Cont)
            end
    end.

register_single_word(State, Rest, Cont) ->
    #{name := Name, sig_in := SigIn0, sig_out := SigOut0, body := Body} = State,
    Guard = maps:get(guard, State, undefined),

    %% Signatures are accumulated left-to-right (Forth convention: leftmost = deepest).
    %% Reverse so element 0 = TOS for match_sig and dispatch.
    SigIn = lists:reverse(SigIn0),
    SigOut = lists:reverse(SigOut0),

    Impl = make_word_impl(Body, Name, SigIn),

    %% Register in the TOS type's dict (first element after reversal).
    TargetType = get_target_type(SigIn),

    NewOp = #operation{
        name = Name,
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl,
        source = {compiled, Body},
        guard = Guard
    },

    ensure_type(TargetType),
    %% Register BEFORE type-checking so the body's self-references resolve.
    af_type:add_op(TargetType, NewOp),

    %% Compile-time type check — self-references now visible in the registry.
    type_check_body(Name, SigIn, SigOut, Body),

    %% Attempt native compilation regardless of guard. The word compiler
    %% translates simple guards (`dup LITERAL CMP`) into Erlang function-head
    %% guards; complex guards cause compile to fail gracefully and the op
    %% stays interpreter-dispatched.
    af_type_any:auto_compile_word(Name),

    %% Sync local dictionary from ETS (auto_compile_word may have replaced ops)
    Dict1 = sync_type_from_ets(TargetType, Cont#continuation.dictionary),

    Cont#continuation{data_stack = Rest, dictionary = Dict1, dispatch_cache = #{}}.

register_multi_word(State, Clauses, Rest, Cont) ->
    #{name := Name, sig_in := MasterSigIn0} = State,
    MasterGuard = maps:get(guard, State, undefined),
    %% Top-level `where` applies to every sub-clause: any sub-clause without
    %% its own guard inherits the master guard; sub-clauses with their own
    %% guard keep it (no implicit conjunction for v1 — per-clause wins).
    Clauses1 = case MasterGuard of
        G when G =/= undefined, G =/= [] ->
            lists:map(fun(C) ->
                case maps:get(guard, C, undefined) of
                    undefined -> C#{guard => MasterGuard};
                    [] -> C#{guard => MasterGuard};
                    _ -> C
                end
            end, Clauses);
        _ -> Clauses
    end,
    MasterSigIn = lists:reverse(MasterSigIn0),
    TargetType = get_target_type(MasterSigIn),
    ensure_type(TargetType),

    %% Register each sub-clause as a separate operation.
    %% Value-constrained clauses come first (they were added in order),
    %% general clauses last — match_first_op tries in list order.
    %% Each clause may carry its own `where` guard.
    HasAnyGuard = lists:any(fun(C) ->
        case maps:get(guard, C, undefined) of
            undefined -> false;
            [] -> false;
            _ -> true
        end
    end, Clauses1),
    %% Two-phase registration so intra-word self-references resolve during
    %% type checking. Phase 1: build all clause ops and register them.
    %% Phase 2: type-check each body with the full clause registry visible.
    ClauseOps = lists:map(fun(#{sig_in := CSigIn0, sig_out := CSigOut0, body := CBody} = C) ->
        CSigIn = lists:reverse(CSigIn0),
        CSigOut = lists:reverse(CSigOut0),
        Impl = make_word_impl(CBody, Name, CSigIn),
        CGuard = maps:get(guard, C, undefined),
        Op = #operation{
            name = Name,
            sig_in = CSigIn,
            sig_out = CSigOut,
            impl = Impl,
            source = {compiled, CBody},
            guard = CGuard
        },
        {CSigIn, CSigOut, CBody, Op}
    end, Clauses1),
    lists:foreach(fun({_SIn, _SOut, _B, Op}) ->
        af_type:add_op(TargetType, Op)
    end, ClauseOps),
    lists:foreach(fun({CSigIn, CSigOut, CBody, _Op}) ->
        type_check_body(Name, CSigIn, CSigOut, CBody)
    end, ClauseOps),

    %% Attempt native compilation regardless of guards. Simple guards compile
    %% to Erlang function-head guards; complex guards cause that clause to be
    %% dropped from the native build and fall back to the interpreter.
    _ = HasAnyGuard,
    af_type_any:auto_compile_word(Name),

    %% Sync local dictionary from ETS
    Dict1 = sync_type_from_ets(TargetType, Cont#continuation.dictionary),

    Cont#continuation{data_stack = Rest, dictionary = Dict1, dispatch_cache = #{}}.

%% Register a word whose body is: pre-dispatch tokens followed by sub-clauses.
%% This pattern (`: foo A B -> C ; pre-body : Match -> ; clause-body . `) used
%% to silently discard `pre-body` on the parasitic compilation path. We
%% preserve it by building a single operation whose body runs pre-dispatch
%% first and then invokes a runtime select-clause dispatcher with the
%% accumulated clauses.
register_predispatch_word(State, Clauses, PreDispatch, Rest, Cont) ->
    #{name := Name, sig_in := MSigIn0, sig_out := MSigOut0} = State,
    Guard = maps:get(guard, State, undefined),
    MSigIn = lists:reverse(MSigIn0),
    MSigOut = lists:reverse(MSigOut0),
    TargetType = get_target_type(MSigIn),
    ensure_type(TargetType),
    %% Normalise clause sigs to TOS-first order.
    ClauseRecs = lists:map(fun(#{sig_in := SIn0, sig_out := SOut0, body := CBody}) ->
        #{sig_in => lists:reverse(SIn0),
          sig_out => lists:reverse(SOut0),
          body => CBody,
          guard => maps:get(guard, State, undefined)}
    end, Clauses),
    SelectOp = make_select_clause_op(ClauseRecs),
    Body = PreDispatch ++ [SelectOp],
    Impl = make_word_impl(Body, Name, MSigIn),
    NewOp = #operation{
        name = Name,
        sig_in = MSigIn,
        sig_out = MSigOut,
        impl = Impl,
        source = {compiled, Body},
        guard = Guard
    },
    af_type:add_op(TargetType, NewOp),
    type_check_predispatch(Name, MSigIn, MSigOut, PreDispatch, ClauseRecs),
    af_type_any:auto_compile_word(Name),
    Dict1 = sync_type_from_ets(TargetType, Cont#continuation.dictionary),
    Cont#continuation{data_stack = Rest, dictionary = Dict1, dispatch_cache = #{}}.

%% Build a runtime select-clause operation. At call time it examines the
%% data stack against each clause's sig_in and runs the first that matches.
make_select_clause_op(ClauseRecs) ->
    #operation{
        name = "<select-clause>",
        sig_in = [],
        sig_out = [],
        impl = fun(C) -> select_clause_dispatch(ClauseRecs, C) end,
        source = {select_clause, ClauseRecs}
    }.

select_clause_dispatch([], Cont) ->
    af_error:raise(no_clause_match,
        "No clause matched the current stack", Cont);
select_clause_dispatch([#{sig_in := SIn, body := CBody} | Rest], Cont) ->
    case af_type:match_sig(SIn, Cont#continuation.data_stack) of
        true -> execute_body(CBody, Cont);
        false -> select_clause_dispatch(Rest, Cont)
    end.

%% Type-check a pre-dispatch + sub-clauses word.
%% Strategy: first, infer the stack after the pre-dispatch body. Then for
%% each clause, build the clause's starting stack by replacing the top-N
%% items of that intermediate stack with the clause's sig_in (so sub-clause
%% value-constraints become visible to the checker), and run the usual
%% defer-or-error check against the main word's declared sig_out.
type_check_predispatch(Name, MSigIn, MSigOut, PreDispatch, ClauseRecs) ->
    Locals = build_virtual_locals(MSigIn),
    case af_type_check:infer_stack(PreDispatch, MSigIn, Locals) of
        {ok, Intermediate} ->
            lists:foreach(fun(#{sig_in := CSigIn, body := CBody}) ->
                N = length(CSigIn),
                case length(Intermediate) >= N of
                    true ->
                        Below = lists:nthtail(N, Intermediate),
                        ClauseStack = CSigIn ++ Below,
                        run_or_defer_check(Name, ClauseStack, MSigOut,
                                           CBody, Locals);
                    false ->
                        %% Pre-dispatch produced a smaller stack than this
                        %% clause's sig expects — latent bug in the source,
                        %% but we defer to the checker for the main word
                        %% rather than raise here (keeps the message in one
                        %% place).
                        run_or_defer_check(Name, CSigIn, MSigOut,
                                           CBody, Locals)
                end
            end, ClauseRecs);
        {error, _} ->
            %% Pre-dispatch body has an unresolvable issue. Defer the
            %% whole word — retry may resolve it later.
            defer_check(Name, MSigIn, MSigOut,
                        PreDispatch ++ [make_select_clause_op(ClauseRecs)],
                        Locals)
    end,
    retry_pending_checks().

%% Run compile-time type check on a word body.
%% Type mismatches are errors when inference is complete (no unknowns).
%% When inference encounters unknown references (forward/mutual recursion,
%% product-type ops not yet registered), defer the check: stash it and retry
%% each time a new word is registered. Unresolved checks at finalisation
%% time will be reported then.
type_check_body(Name, SigIn, SigOut, Body) ->
    Locals = build_virtual_locals(SigIn),
    run_or_defer_check(Name, SigIn, SigOut, Body, Locals),
    %% Any forward-ref defers that this new registration may have unblocked:
    retry_pending_checks().

%% Attempt a single check. On success, ok. On incomplete inference, defer.
%% On fully-resolved mismatch, raise.
run_or_defer_check(Name, SigIn, SigOut, Body, Locals) ->
    Result = af_type_check:check_word(Name, SigIn, SigOut, Body, Locals),
    case Result of
        ok -> ok;
        {error, {type_mismatch, _, #{expected := Expected, actual := Actual}}} ->
            case has_unknown_types(Actual, Expected) of
                true ->
                    defer_check(Name, SigIn, SigOut, Body, Locals);
                false ->
                    error({type_error, Name,
                        lists:flatten(io_lib:format(
                            "Type mismatch in word '~s': "
                            "declared output ~p but inferred ~p",
                            [Name, Expected, Actual]))})
            end;
        {error, {stack_underflow, _OpName, _}} ->
            defer_check(Name, SigIn, SigOut, Body, Locals);
        {error, _Reason} ->
            defer_check(Name, SigIn, SigOut, Body, Locals)
    end.

-define(PENDING_KEY, af_type_check_pending).

defer_check(Name, SigIn, SigOut, Body, Locals) ->
    Pending = case get(?PENDING_KEY) of
        undefined -> #{};
        P -> P
    end,
    Key = {Name, SigIn, SigOut},
    put(?PENDING_KEY, Pending#{Key => {Body, Locals}}),
    ok.

%% Retry every pending check. Any that now resolve are removed.
%% Fixed-point: keep retrying while the pending set shrinks.
retry_pending_checks() ->
    case get(?PENDING_KEY) of
        undefined -> ok;
        Pending when map_size(Pending) =:= 0 -> ok;
        Pending ->
            Before = map_size(Pending),
            put(?PENDING_KEY, #{}),
            maps:foreach(fun({N, SI, SO}, {B, L}) ->
                run_or_defer_check(N, SI, SO, B, L)
            end, Pending),
            After = case get(?PENDING_KEY) of
                undefined -> 0;
                P2 -> map_size(P2)
            end,
            case After < Before of
                true -> retry_pending_checks();
                false -> ok
            end
    end.

%% Return the list of pending type checks — words whose bodies reference
%% unresolved names. Each entry: {Name, SigIn, SigOut}. Lets the REPL or
%% test harness surface unresolved references.
list_pending_checks() ->
    case get(?PENDING_KEY) of
        undefined -> [];
        P -> maps:keys(P)
    end.

%% Force one last retry pass. If anything remains pending, print a
%% warning listing the unresolved names and return the remaining list.
%% Never raises — keeps the REPL alive.
finalize_pending_checks() ->
    retry_pending_checks(),
    case list_pending_checks() of
        [] -> ok;
        Pending ->
            io:format("~n"
                      "Warning: ~p type check(s) remain unresolved — "
                      "references were never defined:~n",
                      [length(Pending)]),
            lists:foreach(fun({Name, SigIn, SigOut}) ->
                io:format("  ~s  (~p -> ~p)~n", [Name, SigIn, SigOut])
            end, Pending),
            {pending, Pending}
    end.

%% Drop the pending list. Useful for tests that want a clean slate.
clear_pending_checks() ->
    put(?PENDING_KEY, #{}),
    ok.

%% Build the virtual locals map that the type checker uses to resolve
%% auto-field-binding tokens (.x, ..y, bare field names). Mirrors the
%% structure of build_frame/3 at runtime but records field *types* rather
%% than tagged values.
build_virtual_locals(SigIn) ->
    ProductSlots = find_product_slots(SigIn),
    N = length(SigIn),
    build_virtual_frame(ProductSlots, N).

build_virtual_frame([], _N) -> #{};
build_virtual_frame(ProductSlots, N) ->
    AllEntries = lists:flatmap(fun({StoredPos, _Type, Fields}) ->
        SourcePos = N - StoredPos,
        DotPrefix = lists:duplicate(SourcePos, $.),
        [{DotPrefix ++ FName, FName, FType}
         || {FName, _FIdx, FType} <- Fields]
    end, ProductSlots),
    BareCounts = lists:foldl(fun({_D, Bare, _T}, Acc) ->
        maps:update_with(Bare, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, AllEntries),
    DottedFrame = lists:foldl(fun({Dotted, _Bare, T}, Acc) ->
        Acc#{Dotted => T}
    end, #{}, AllEntries),
    lists:foldl(fun({_D, Bare, T}, Acc) ->
        case maps:get(Bare, BareCounts) of
            1 -> Acc#{Bare => T};
            _ -> Acc
        end
    end, DottedFrame, AllEntries).

%% Check if inferred types suggest incomplete inference:
%% - Contains 'Atom' from unresolved tokens (product type ops, etc.)
%% - Different stack depth from expected (missing type info)
%% - Expected types include non-builtin types the checker can't verify
%% Classify an inferred-vs-declared mismatch.
%% Returns `true` only when the inference genuinely could not complete:
%%   - the inferred stack contains 'Atom' (a forward-reference fallback), or
%%   - the declared sig mentions a non-builtin custom type that hasn't
%%     been registered yet.
%% A pure length-mismatch or a clean concrete-type mismatch is a real
%% error; we return `false` so the caller raises.
has_unknown_types(Types, Expected) ->
    Builtins = ['Any', '_', 'Int', 'Float', 'Bool', 'String', 'Atom',
                'List', 'Map', 'Tuple', 'Actor', 'Message', 'ListBuilder',
                'Ref'],
    HasAtomFallback = lists:any(fun
        ('Atom') -> true;
        (_) -> false
    end, Types),
    HasUnknownExpected = lists:any(fun
        ({_T, _V}) -> false;
        (T) ->
            not lists:member(T, Builtins)
            andalso not af_type_check:is_type_variable(T)
            andalso af_type:get_type(T) =:= not_found
    end, Expected),
    HasAtomFallback orelse HasUnknownExpected.

get_target_type([{Type, _Value} | _]) -> Type;
get_target_type([Type | _]) when is_atom(Type) ->
    %% Type variables (like '_a') and the wildcard '_' aren't concrete
    %% types. A word whose TOS input is polymorphic registers on 'Any'
    %% so it stays reachable from any runtime TOS type.
    case af_type_check:is_type_variable(Type) orelse Type =:= '_' orelse Type =:= 'Any' of
        true -> 'Any';
        false -> Type
    end;
get_target_type([]) -> 'Any'.

ensure_type(TypeName) ->
    case af_type:get_type(TypeName) of
        not_found -> af_type:register_type(#af_type{name = TypeName});
        _ -> ok
    end.

%% Re-read a type's full definition from ETS into the local dictionary.
%% Called after auto_compile_word which may replace ops in ETS with native wrappers.
sync_type_from_ets(_TypeName, undefined) -> undefined;
sync_type_from_ets(TypeName, Dict) ->
    case af_type:get_type(TypeName) of
        {ok, Type} -> maps:put(TypeName, Type, Dict);
        _ -> Dict
    end.

%% Build an execution function from a compiled word body.
%% For tail-recursive words (last body token is a self-call), we pop the
%% trace frame BEFORE the tail call so the self-call is in Erlang tail
%% position, allowing the BEAM's native TCO to prevent stack growth.
%% When sig_in contains a product type, the returned impl wraps body
%% execution in a locals-frame push/pop that exposes each product's
%% fields as bare identifiers (when unambiguous) plus always as
%% positional dot-prefix forms (.x for source-leftmost, ..x for next, etc.)
make_word_impl(Body, WordName, SigIn) ->
    ProductSlots = find_product_slots(SigIn),
    Base = case detect_tail_call(Body, WordName) of
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
    end,
    case ProductSlots of
        []    -> Base;
        Slots ->
            case body_uses_bindings(Body, Slots) of
                true ->
                    %% Compute the frame plan ONCE at registration
                    %% and hand a flat plan to a fast call-time
                    %% builder. Saves four list traversals per word
                    %% entry (was ~5% of hot-loop runtime).
                    Plan = make_frame_plan(Slots, length(SigIn)),
                    wrap_with_locals_planned(Base, Plan);
                false -> Base
            end
    end.


%% Pre-compute the frame entries at registration time. Group entries
%% by product position so runtime does one lists:nth per product rather
%% than per field. Shape: [{StoredPos, [{[Key,...], Idx, FType}, ...]}].
make_frame_plan(ProductSlots, N) ->
    AllEntries =
        lists:flatmap(fun({StoredPos, _Type, Fields}) ->
            SourcePos = N - StoredPos,
            DotPrefix = lists:duplicate(SourcePos, $.),
            [{DotPrefix ++ FName, FName, StoredPos, FIdx, FType}
             || {FName, FIdx, FType} <- Fields]
        end, ProductSlots),
    BareCounts =
        lists:foldl(fun({_D, Bare, _P, _I, _T}, Acc) ->
            maps:update_with(Bare, fun(C) -> C + 1 end, 1, Acc)
        end, #{}, AllEntries),
    Flat =
        [begin
            %% When bare name is globally unambiguous, the dotted
            %% positional form is functionally equivalent and almost
            %% never referenced — skip it to halve the frame size.
            %% When ambiguous, only the dotted form survives.
            Keys = case maps:get(Bare, BareCounts) of
                1 -> [Bare];
                _ -> [Dotted]
            end,
            {Pos, Keys, Idx, FType}
         end || {Dotted, Bare, Pos, Idx, FType} <- AllEntries],
    %% Group by Pos, preserving order within each group.
    Grouped = lists:foldl(fun({Pos, Keys, Idx, FType}, Acc) ->
        Existing = maps:get(Pos, Acc, []),
        maps:put(Pos, Existing ++ [{Keys, Idx, FType}], Acc)
    end, #{}, Flat),
    [{Pos, Fields} || {Pos, Fields} <- maps:to_list(Grouped)].

%% Wrap with pre-computed frame plan. Compile the plan to a flat list
%% of {Key, Pos, Idx, FType} tuples and drive it with a tight loop —
%% avoids the per-field nested lists:foldl and the add_keys walk that
%% dominated ~20% of the hot loop in profiling.
wrap_with_locals_planned(BaseImpl, Plan) ->
    FlatEntries = flatten_plan(Plan),
    fun(Cont) ->
        Frame = build_frame_flat(FlatEntries,
                                 Cont#continuation.data_stack, #{}),
        Cont1 = Cont#continuation{locals = [Frame | Cont#continuation.locals]},
        %% Mirror the locals stack into the process dict so native-
        %% compiled callees can do caller-frame binding lookups through
        %% af_compile:apply_impl/2 (which doesn't see Cont.locals
        %% directly). Fixes #160 — send-to-parent style words whose
        %% body references a binding from an enclosing HosSelf frame
        %% can now native-compile without atomizing the binding name.
        OldProc = case erlang:get(af_locals_stack) of
            undefined -> [];
            LS -> LS
        end,
        erlang:put(af_locals_stack, [Frame | OldProc]),
        try
            Cont2 = BaseImpl(Cont1),
            [_PoppedFrame | LocalsRest] = Cont2#continuation.locals,
            erlang:put(af_locals_stack, OldProc),
            Cont2#continuation{locals = LocalsRest}
        catch
            Class:Reason:CallStack ->
                erlang:put(af_locals_stack, OldProc),
                erlang:raise(Class, Reason, CallStack)
        end
    end.

%% Flatten a grouped plan ([{Pos, [{Keys, Idx, FType}, ...]}]) into a
%% flat list ([{Key, Pos, Idx, FType}, ...]) so the runtime does one
%% put per (key, value) instead of nested folds + add_keys.
flatten_plan(Plan) ->
    lists:flatmap(fun({Pos, Fields}) ->
        lists:flatmap(fun({Keys, Idx, FType}) ->
            [{K, Pos, Idx, FType} || K <- Keys]
        end, Fields)
    end, Plan).

build_frame_flat([], _DataStack, Frame) ->
    Frame;
build_frame_flat([{Key, Pos, Idx, FType} | Rest], DataStack, Frame) ->
    Product = nth_opt(Pos, DataStack),
    RawVal = element(Idx + 2, Product),
    Tagged = normalize_binding(FType, RawVal),
    build_frame_flat(Rest, DataStack, Frame#{Key => Tagged}).

%% lists:nth with 0-indexed fast path for Pos 0/1/2 (the only values
%% produced in practice — sig_in rarely exceeds 3 products).
nth_opt(0, [H | _]) -> H;
nth_opt(1, [_, H | _]) -> H;
nth_opt(2, [_, _, H | _]) -> H;
nth_opt(N, L) -> lists:nth(N + 1, L).

%% Kept for any external callers still referencing the old shape.
build_frame_from_plan(Plan, DataStack) ->
    build_frame_flat(flatten_plan(Plan), DataStack, #{}).

add_keys([], _V, Frame)   -> Frame;
add_keys([K | Ks], V, Frame) -> add_keys(Ks, V, Frame#{K => V}).

%% Return true iff any body token is one of the bound names
%% (bare field name OR dot-prefixed positional form).
body_uses_bindings(Body, ProductSlots) ->
    Names = binding_names(ProductSlots),
    case Names of
        []   -> false;
        _    ->
            NameSet = sets:from_list(Names),
            lists:any(fun(Op) ->
                case Op of
                    #operation{name = N} -> sets:is_element(N, NameSet);
                    _ -> false
                end
            end, Body)
    end.

binding_names(ProductSlots) ->
    %% Collect bare + dotted forms as a flat list. lists:flatten over
    %% strings (list of chars) would over-flatten; use append instead.
    lists:append(
      [ [FName | [dot_prefix(DotLen) ++ FName || DotLen <- lists:seq(1, 9)]]
        || {_StoredPos, _Type, Fields} <- ProductSlots,
           {FName, _FIdx, _FType} <- Fields ]).

dot_prefix(N) -> lists:duplicate(N, $.).

%% Scan a sig_in list and return [{StoredPos, TypeName, Fields}] for each
%% product type, where Fields = [{FieldName::string, FieldIdx::non_neg, FieldType::atom}].
%% Stored position is 0-indexed from TOS. Empty/non-product / value-constrained
%% entries are skipped.
find_product_slots([]) -> [];
find_product_slots(SigIn) ->
    Indexed = lists:zip(lists:seq(0, length(SigIn) - 1), SigIn),
    lists:filtermap(fun
        ({Pos, Type}) when is_atom(Type) ->
            case lookup_fields(Type) of
                {ok, Fields} when Fields =/= [] ->
                    FieldInfo = lists:zip3(
                        [atom_to_list(FN) || {FN, _FT} <- Fields],
                        lists:seq(0, length(Fields) - 1),
                        [FT || {_FN, FT} <- Fields]),
                    %% FieldInfo entries: {FieldName, FieldIdx, FieldType}
                    {true, {Pos, Type,
                            [{N, I, T} || {N, I, T} <- FieldInfo]}};
                _ -> false
            end;
        (_) -> false
    end, Indexed).

lookup_fields(TypeName) ->
    case af_type:get_type(TypeName) of
        {ok, #af_type{fields = Fields}} when Fields =/= [] -> {ok, Fields};
        _ -> error
    end.

%% Wrap a base impl with a locals-frame push/pop. N = total sig_in length
%% (needed to compute source positions for dot-prefix keys).
wrap_with_locals(BaseImpl, ProductSlots, N) ->
    fun(Cont) ->
        Frame = build_frame(ProductSlots, N, Cont#continuation.data_stack),
        Cont1 = Cont#continuation{locals = [Frame | Cont#continuation.locals]},
        try
            Cont2 = BaseImpl(Cont1),
            [_PoppedFrame | LocalsRest] = Cont2#continuation.locals,
            Cont2#continuation{locals = LocalsRest}
        catch
            Class:Reason:Stack ->
                %% Ensure locals frame doesn't leak on exception
                erlang:raise(Class, Reason, Stack)
        end
    end.

%% Build the locals frame map. Always include positional `.field` keys
%% (source-position 1 = 1 dot, 2 = 2 dots, ...). Add bare field names
%% only when unambiguous across all product slots. Compile-time shadowing
%% checks could land here in a follow-up.
build_frame(ProductSlots, N, DataStack) ->
    %% Collect every field access with its stored-position and source-position.
    AllEntries = lists:flatmap(fun({StoredPos, _Type, Fields}) ->
        SourcePos = N - StoredPos,
        DotPrefix = lists:duplicate(SourcePos, $.),
        Product = lists:nth(StoredPos + 1, DataStack),
        [begin
            RawVal = element(FIdx + 2, Product),
            Tagged = normalize_binding(FType, RawVal),
            {DotPrefix ++ FName, FName, Tagged}
         end || {FName, FIdx, FType} <- Fields]
    end, ProductSlots),
    %% Count bare-name occurrences to decide which are unambiguous.
    BareCounts = lists:foldl(fun({_Dotted, Bare, _V}, Acc) ->
        maps:update_with(Bare, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, AllEntries),
    %% Always include dotted keys.
    DottedFrame = lists:foldl(fun({Dotted, _, V}, Acc) ->
        Acc#{Dotted => V}
    end, #{}, AllEntries),
    %% Add bare names only when they appear once across all slots.
    lists:foldl(fun({_, Bare, V}, Acc) ->
        case maps:get(Bare, BareCounts) of
            1 -> Acc#{Bare => V};
            _ -> Acc
        end
    end, DottedFrame, AllEntries).

%% Normalise a field value for binding so the resulting stack item
%% follows the same convention as af_term:to_stack_item: Atom values
%% are stored as strings (lists). Product instances constructed from
%% Erlang code (e.g. the HOS wire format) may carry raw atoms in an
%% 'Atom'-typed field; bindings should present them in the same shape
%% that `elt` / `to-stack-item` produce so consumers that compare or
%% make-tuple them don't hit a list_to_atom crash on an atom value.
normalize_binding('Atom', A) when is_atom(A) -> {'Atom', atom_to_list(A)};
normalize_binding('Atom', B) when is_binary(B) -> {'Atom', binary_to_list(B)};
normalize_binding(FType, RawVal) when is_tuple(RawVal), tuple_size(RawVal) >= 2 ->
    %% A raw tuple field whose first element names a registered product
    %% type is itself a flat-tuple product instance — push as-is so
    %% body tokens see it with its real TOS type, not as a doubly-
    %% tagged {FType, {FType, ...}} pair.
    Head = element(1, RawVal),
    case is_atom(Head) andalso is_flat_product_tuple(Head, RawVal) of
        true  -> RawVal;
        false -> {FType, RawVal}
    end;
normalize_binding(FType, RawVal) -> {FType, RawVal}.

is_flat_product_tuple(Head, Tuple) ->
    case catch af_type:get_type(Head) of
        {ok, #af_type{fields = Fs}}
            when Fs =/= [], length(Fs) =:= tuple_size(Tuple) - 1 -> true;
        _ -> false
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
        not_found ->
            %% Master didn't recognise it. Fall through to common literal
            %% detection so tokens like True / False / 3 / 3.14 are typed
            %% correctly even when the master position is some other type.
            case try_common_literals(TokenValue) of
                {ok, {Type, Value}} -> {Type, Value};
                not_found -> list_to_atom(TokenValue)
            end
    end.

%% Resolve sub-clause tokens without any master-sig anchoring. Used for
%% pre-dispatch-style words, where the sub-clause matches against an
%% intermediate stack whose types aren't known until pre-dispatch has been
%% type-inferred. Each token is resolved on its own — quoted strings
%% become String literals, and common literals (Int / Float / Bool) are
%% recognised directly; everything else falls back to an atom value.
resolve_sub_tokens_independent(RawTokens) ->
    lists:map(fun
        ({TokenValue, true}) ->
            {'String', list_to_binary(TokenValue)};
        ({TokenValue, false}) ->
            case try_common_literals(TokenValue) of
                {ok, {Type, Value}} -> {Type, Value};
                not_found -> list_to_atom(TokenValue)
            end
    end, RawTokens).

try_common_literals(TokenValue) ->
    try_literals_seq(TokenValue, ['Int', 'Float', 'Bool']).

try_literals_seq(_TokenValue, []) -> not_found;
try_literals_seq(TokenValue, [Type | Rest]) ->
    case try_literal_for_type(TokenValue, Type) of
        {ok, _} = Ok -> Ok;
        not_found -> try_literals_seq(TokenValue, Rest)
    end.

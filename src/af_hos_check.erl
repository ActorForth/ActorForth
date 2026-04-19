-module(af_hos_check).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([check_system/1, check_system_raise/1, init/0]).
-export([register_system/1, lookup_system/1, clear_registry/0,
         registered_child_events/1]).

-define(REG_KEY, hos_system_registry).

%% Axiom checker for HOS SystemNode trees.
%%
%% Walks a SystemNode tree and verifies Hamilton's control axioms
%% by construction:
%%
%%   Axiom 1 (immediate-only control). Every identifier referenced in
%%           a handler body must resolve within the system's own
%%           scope: its parent system, its direct children, its
%%           declared event names, its transition state names, or an
%%           a4 builtin. Anything else  -  a sibling system's name, a
%%           distant cousin, a user-defined helper from outside the
%%           tree  -  is an axiom-1 violation.
%%
%% Returns {ok, []} when no violations exist, otherwise
%% {violations, [String]} where each string cites the offending
%% system path, handler, and token.
%%
%% Further axioms (transition-table completeness, TMap reachability)
%% will be added as the DSL surface grows to require them.

init() ->
    clear_registry(),
    af_type:add_op('Any', #operation{
        name = "check-system",
        sig_in = ['SystemNode'],
        sig_out = ['List'],
        impl = fun op_check_system/1
    }).

%% Simple per-process registry of system definitions. Used so that a
%% parent system's scope can include events declared on its direct
%% children (Axiom 1's "parent invokes children" aspect). Populated
%% by the DSL when a `system ... end` block completes.
%%
%% Before registering, we run a consistency check: any child this
%% system declares that is already registered must list THIS system
%% as its parent, and any parent this system declares that is
%% already registered must list THIS system in its children. A
%% mismatch raises {axiom_violation, _} so the inconsistency
%% surfaces at definition time rather than as a silent drift.
register_system({'SystemNode', NameBin, _, _, _, _, _} = Sys) ->
    check_parent_children_consistency(Sys),
    Name = binary_to_list(NameBin),
    Reg = case get(?REG_KEY) of
        undefined -> #{};
        R -> R
    end,
    put(?REG_KEY, Reg#{Name => Sys}),
    ok.

%% Check parent/children symmetry in BOTH directions against every
%% system currently in the registry. Runs BEFORE this system is
%% added, so any already-registered system whose claims conflict
%% with ours surfaces now.
check_parent_children_consistency({'SystemNode', NameBin, ParentNameBin,
                                   _, Children, _, _}) ->
    Name = binary_to_list(NameBin),
    Parent = binary_to_list(ParentNameBin),
    MyChildNames = [binary_to_list(element(2, C)) || C <- Children],
    Reg = case get(?REG_KEY) of
        undefined -> #{};
        R -> R
    end,
    V1 = children_i_declare_check(Name, MyChildNames, Reg),
    V2 = my_parent_check(Name, Parent, Reg),
    V3 = others_who_list_me_check(Name, Parent, Reg),
    V4 = others_who_claim_me_as_parent_check(Name, MyChildNames, Reg),
    case V1 ++ V2 ++ V3 ++ V4 of
        [] -> ok;
        Violations ->
            Msg = "HOS parent/children consistency errors:\n  " ++
                  lists:flatten(lists:join("\n  ", Violations)),
            error({axiom_violation, Msg})
    end.

%% (1) Any child I declare that's already registered must name me as parent.
children_i_declare_check(Name, MyChildNames, Reg) ->
    lists:flatmap(fun(CN) ->
        case maps:find(CN, Reg) of
            {ok, CSys} ->
                CP = binary_to_list(element(3, CSys)),
                case CP =:= Name of
                    true -> [];
                    false ->
                        [lists:flatten(io_lib:format(
                            "system '~s' lists child '~s', but '~s' "
                            "declares parent='~s' not '~s'",
                            [Name, CN, CN, CP, Name]))]
                end;
            error -> []
        end
    end, MyChildNames).

%% (2) If my parent is registered, I must be in its children list.
my_parent_check(_Name, "", _Reg) -> [];
my_parent_check(Name, Parent, Reg) ->
    case maps:find(Parent, Reg) of
        {ok, PSys} ->
            PChildren = [binary_to_list(element(2, C))
                         || C <- element(5, PSys)],
            case lists:member(Name, PChildren) of
                true -> [];
                false ->
                    [lists:flatten(io_lib:format(
                        "system '~s' declares parent='~s', but '~s' "
                        "does not list '~s' in its children",
                        [Name, Parent, Parent, Name]))]
            end;
        error -> []
    end.

%% (3) Any already-registered system that lists me as a child must BE my parent.
others_who_list_me_check(Name, Parent, Reg) ->
    maps:fold(fun(OName, OSys, Acc) ->
        OChildren = [binary_to_list(element(2, C))
                     || C <- element(5, OSys)],
        case lists:member(Name, OChildren) andalso OName =/= Parent of
            true ->
                [lists:flatten(io_lib:format(
                    "system '~s' lists '~s' as a child, but '~s' "
                    "declares parent='~s' not '~s'",
                    [OName, Name, Name, Parent, OName])) | Acc];
            false -> Acc
        end
    end, [], Reg).

%% (4) Any already-registered system whose parent is me must be in my children.
others_who_claim_me_as_parent_check(Name, MyChildNames, Reg) ->
    maps:fold(fun(OName, OSys, Acc) ->
        OP = binary_to_list(element(3, OSys)),
        case OP =:= Name andalso not lists:member(OName, MyChildNames) of
            true ->
                [lists:flatten(io_lib:format(
                    "system '~s' declares parent='~s', but '~s' "
                    "does not list '~s' in its children",
                    [OName, Name, Name, OName])) | Acc];
            false -> Acc
        end
    end, [], Reg).

lookup_system(Name) ->
    Reg = case get(?REG_KEY) of
        undefined -> #{};
        R -> R
    end,
    maps:get(Name, Reg, not_found).

clear_registry() ->
    put(?REG_KEY, #{}),
    ok.

%% Given a child name, return the list of event-name strings declared
%% on it. Returns [] if the child isn't in the registry yet (which is
%% legitimate in forward-reference cases  -  the parent's scope just
%% doesn't get the child's events until the child is defined).
registered_child_events(ChildName) ->
    case lookup_system(ChildName) of
        not_found -> [];
        {'SystemNode', _, _, _, _, Handlers, _} ->
            [atom_to_list(element(2, H)) || H <- Handlers]
    end.

op_check_system(Cont) ->
    [Instance | Rest] = Cont#continuation.data_stack,
    Violations = violations(Instance),
    Items = [{'String', list_to_binary(V)} || V <- Violations],
    Cont#continuation{data_stack = [{'List', Items} | Rest]}.

%% Public: return violations as a list of strings.
check_system(SystemInstance) ->
    Violations = violations(SystemInstance),
    case Violations of
        [] -> {ok, []};
        _ -> {violations, Violations}
    end.

%% Public: raise on violations, so a system definition fails fast
%% when it would have violated an axiom.
check_system_raise(SystemInstance) ->
    case check_system(SystemInstance) of
        {ok, []} -> ok;
        {violations, Violations} ->
            Msg = "HOS axiom violations:\n  " ++
                  lists:flatten(lists:join("\n  ", Violations)),
            error({axiom_violation, Msg})
    end.

%% ---------------------------------------------------------------
%% Walk
%% ---------------------------------------------------------------

violations(Instance) ->
    walk(Instance, "").

walk({'SystemNode', NameRaw, ParentRaw, StateType,
      Children, Handlers, Transitions}, PathPrefix) ->
    Name = to_list(NameRaw),
    Path = path(PathPrefix, Name),
    Scope = build_scope(Name, ParentRaw, StateType, Children,
                         Handlers, Transitions),
    ChildNames = child_names(Children),
    ChildEvents = lists:flatmap(fun registered_child_events/1, ChildNames),
    ParentStr = case parent_entries(ParentRaw) of
                    [] -> "";
                    [P] -> P
                end,
    TransitionV = check_transition_effects(Transitions, Scope, Path),
    HandlerV = lists:flatmap(
        fun(H) ->
            ScopeV = check_handler(H, Scope, Path),
            TransV = check_handler_transitions(H, Transitions, Path),
            StatefulV = check_stateful_body(H, StateType, ParentStr,
                                            ChildNames, ChildEvents, Path),
            ScopeV ++ TransV ++ StatefulV
        end,
        Handlers),
    ChildV = lists:flatmap(
        fun(C) -> walk(C, Path) end,
        Children),
    TransitionV ++ HandlerV ++ ChildV.

%% Axiom 1 for declared effects: effect_target (if present) must be
%% in the system's scope. A transition that invokes a subsystem name
%% the system cannot see is an out-of-scope reference just like a
%% body token would be.
check_transition_effects(Transitions, Scope, Path) ->
    lists:flatmap(fun(T) -> check_transition_effect(T, Scope, Path) end,
                  Transitions).

check_transition_effect({'TransitionSpec', From, To, Trigger,
                         EffectTarget, _EffectEvent}, Scope, Path) ->
    case effect_target_str(EffectTarget) of
        "" -> [];
        Target ->
            case token_in_scope(Target, Scope) of
                true -> [];
                false ->
                    [lists:flatten(io_lib:format(
                        "axiom_1: system '~s' declares transition "
                        "~s -> ~s under trigger '~s' with effect "
                        "on '~s', but '~s' is not in scope",
                        [Path, From, To, Trigger, Target, Target]))]
            end
    end;
%% Backwards compatibility for hand-built 4-tuple TransitionSpecs in
%% older tests. Treated as having no effect.
check_transition_effect({'TransitionSpec', _, _, _}, _Scope, _Path) ->
    [].

effect_target_str(<<>>) -> "";
effect_target_str("") -> "";
effect_target_str(B) when is_binary(B) -> binary_to_list(B);
effect_target_str(L) when is_list(L) -> L.

%% A stateful system's handler bodies must contain only state
%% markers and passive operations. Any reference to a child, the
%% parent, or a child's event name is an axiom violation: subsystem
%% interactions live on transitions (as declared effects), never
%% hidden inside a handler body. Reorderings of subsystem calls are
%% therefore visible in the transitions table, not buried in an
%% imperative sequence.
check_stateful_body(_Handler, 'None', _Parent, _Children, _CEvents, _Path) ->
    [];
check_stateful_body({'HandlerSpec', EventName, _SigIn, _SigOut, Body},
                    _StateType, ParentStr, ChildNames, ChildEvents, Path) ->
    EventStr = atom_to_list(EventName),
    Forbidden = sets:from_list(
        case ParentStr of
            "" -> [];
            P  -> [P]
        end ++ ChildNames ++ ChildEvents),
    Tokens = [token_text(T) || T <- Body],
    scan_stateful_body(Tokens, Forbidden, EventStr, Path).

scan_stateful_body([], _Forbidden, _Event, _Path) ->
    [];
scan_stateful_body(["->", _State | Rest], Forbidden, Event, Path) ->
    scan_stateful_body(Rest, Forbidden, Event, Path);
scan_stateful_body([Token | Rest], Forbidden, Event, Path) ->
    case sets:is_element(Token, Forbidden) of
        true ->
            [format_stateful_body_violation(Path, Event, Token)
             | scan_stateful_body(Rest, Forbidden, Event, Path)];
        false ->
            scan_stateful_body(Rest, Forbidden, Event, Path)
    end.

format_stateful_body_violation(Path, Event, Token) ->
    lists:flatten(io_lib:format(
        "axiom_5: system '~s' handler '~s' references subsystem '~s' "
        "in its body; stateful systems must declare subsystem "
        "interactions as transition effects (: Target event), not "
        "inline. Move this to the transitions table.",
        [Path, Event, Token])).

path("", Name) -> Name;
path(Prefix, Name) -> Prefix ++ "." ++ Name.

%% ---------------------------------------------------------------
%% Scope construction  -  what names a system body may reference.
%% ---------------------------------------------------------------

build_scope(SelfName, ParentRaw, StateType, Children, Handlers, Transitions) ->
    ChildNames = child_names(Children),
    %% Parent-to-child control (Axiom 1's "parent invokes children")
    %% means a system may reference any event declared on one of its
    %% direct children. Pull those events from the registry so the
    %% scope check allows e.g. `Door close` in Car's body.
    ChildEvents = lists:flatmap(fun registered_child_events/1, ChildNames),
    Entries =
        parent_entries(ParentRaw)
        ++ [SelfName]
        ++ ChildNames
        ++ ChildEvents
        ++ state_type_entries(StateType)
        ++ handler_entries(Handlers)
        ++ transition_entries(Transitions),
    sets:from_list(Entries).

child_names(Children) ->
    [to_list(element(2, C)) || C <- Children].

parent_entries(<<>>) -> [];
parent_entries(B) when is_binary(B) -> [binary_to_list(B)];
parent_entries([]) -> [];
parent_entries(S) when is_list(S) -> [S].

state_type_entries('None') -> [];
state_type_entries(T) when is_atom(T) -> [atom_to_list(T)];
state_type_entries(_) -> [].

handler_entries(Handlers) ->
    [atom_to_list(element(2, H)) || H <- Handlers].

transition_entries(Transitions) ->
    lists:flatmap(fun(T) ->
        [atom_to_list(A) || A <- [element(2, T), element(3, T), element(4, T)]]
    end, Transitions).

%% ---------------------------------------------------------------
%% Handler body check
%% ---------------------------------------------------------------

check_handler({'HandlerSpec', EventName, _SigIn, _SigOut, Body},
              Scope, Path) ->
    EventStr = atom_to_list(EventName),
    Tokens = [token_text(T) || T <- Body],
    scope_walk(Tokens, Scope, EventStr, Path).

%% Walk body tokens for axiom-1 (scope) violations. Treat `->` + the
%% following token as a transition marker; the axiom-5 pass handles
%% whether the transition itself is legal.
scope_walk([], _Scope, _Event, _Path) ->
    [];
scope_walk(["->", _StateName | Rest], Scope, Event, Path) ->
    scope_walk(Rest, Scope, Event, Path);
scope_walk([Token | Rest], Scope, Event, Path) ->
    case token_in_scope(Token, Scope) of
        true -> scope_walk(Rest, Scope, Event, Path);
        false ->
            [format_violation(Path, Event, Token)
             | scope_walk(Rest, Scope, Event, Path)]
    end.

%% Axiom 5: walk the body's `-> State` markers and verify each
%% transition is declared in the system's transitions table with the
%% current handler as trigger. Start state is the set of valid source
%% states for this handler's trigger. Each marker narrows to the
%% target state; the next marker checks from there.
check_handler_transitions({'HandlerSpec', EventName, _SigIn, _SigOut, Body},
                          Transitions, Path) ->
    EventStr = atom_to_list(EventName),
    Tokens = [token_text(T) || T <- Body],
    %% (No debug output.)
    %% Entry states for the handler are Froms that do NOT appear as
    %% Tos for transitions carrying the same trigger; by definition,
    %% they are the states from which the handler's work can begin
    %% externally. Continuation transitions (From -> To with trigger
    %% equal to the handler name, but From is also a To) are internal
    %% steps within one handler run.
    EntryStates = compute_entry_states(EventStr, Transitions),
    walk_transitions(Tokens, undefined, EntryStates, EventStr,
                     Transitions, Path).

%% Entry states for a handler are the Froms of transitions carrying
%% the handler's trigger that DO NOT also appear as a To for the
%% same trigger (those are continuations of one handler run). If
%% the set is empty -- which happens when the state machine has a
%% cycle under one trigger -- fall back to the From of the first
%% declared transition for that trigger, treating it as the
%% intended entry.
compute_entry_states(EventStr, Transitions) ->
    Matching = [T || T <- Transitions,
                      atom_to_list(element(4, T)) =:= EventStr],
    ToStates = [atom_to_list(element(3, T)) || T <- Matching],
    FromStates = [atom_to_list(element(2, T)) || T <- Matching],
    NonContinuation = [F || F <- FromStates, not lists:member(F, ToStates)],
    case NonContinuation of
        [] ->
            case Matching of
                [] -> [];
                [First | _] -> [atom_to_list(element(2, First))]
            end;
        _ -> NonContinuation
    end.

walk_transitions([], _CurState, _EntryStates, _Event, _Trans, _Path) ->
    [];
walk_transitions(["->", Target | Rest], CurState, EntryStates, Event,
                  Trans, Path) ->
    case CurState of
        undefined ->
            %% First `-> Target`. An entry state E must exist in the
            %% table such that (E, Target, Event) is declared.
            case entry_transition_exists(EntryStates, Target, Event, Trans) of
                true ->
                    walk_transitions(Rest, Target, EntryStates,
                                     Event, Trans, Path);
                false ->
                    [format_axiom5_entry(Path, Event, Target, EntryStates)
                     | walk_transitions(Rest, Target, EntryStates,
                                        Event, Trans, Path)]
            end;
        _ ->
            case has_transition_under(CurState, Target, Event, Trans) of
                true ->
                    walk_transitions(Rest, Target, EntryStates,
                                     Event, Trans, Path);
                false ->
                    [format_axiom5_step(Path, Event, CurState, Target)
                     | walk_transitions(Rest, Target, EntryStates,
                                        Event, Trans, Path)]
            end
    end;
walk_transitions([_Other | Rest], CurState, EntryStates, Event, Trans, Path) ->
    walk_transitions(Rest, CurState, EntryStates, Event, Trans, Path).

entry_transition_exists(EntryStates, Target, Event, Transitions) ->
    lists:any(fun(E) ->
        lists:any(fun(T) ->
            atom_to_list(element(2, T)) =:= E andalso
            atom_to_list(element(3, T)) =:= Target andalso
            atom_to_list(element(4, T)) =:= Event
        end, Transitions)
    end, EntryStates).

%% Axiom 5 step-check: the (From, To) pair must be declared under
%% the same trigger as the enclosing handler. Previously this check
%% ignored the trigger, so `-> Opening` under event `shutdown` could
%% reuse an edge declared under `open`. That is a soundness hole.
has_transition_under(From, To, Trigger, Transitions) ->
    lists:any(fun(T) ->
        atom_to_list(element(2, T)) =:= From andalso
        atom_to_list(element(3, T)) =:= To andalso
        atom_to_list(element(4, T)) =:= Trigger
    end, Transitions).

format_axiom5_entry(Path, Event, Target, EntryStates) ->
    lists:flatten(io_lib:format(
        "axiom_5: system '~s' handler '~s' starts with transition "
        "to '~s' but no declared entry state ~p reaches '~s' with "
        "trigger '~s'",
        [Path, Event, Target, EntryStates, Target, Event])).

format_axiom5_step(Path, Event, From, To) ->
    lists:flatten(io_lib:format(
        "axiom_5: system '~s' handler '~s' attempts undeclared "
        "transition ~s -> ~s",
        [Path, Event, From, To])).

token_text({'String', B}) -> binary_to_list(B);
token_text(S) when is_list(S) -> S;
token_text(B) when is_binary(B) -> binary_to_list(B);
token_text({'Atom', A}) -> atom_to_list(A);
token_text(A) when is_atom(A) -> atom_to_list(A).

token_in_scope(Token, Scope) ->
    sets:is_element(Token, Scope)
        orelse is_literal(Token)
        orelse is_a4_builtin(Token).

%% Numeric / boolean / quoted-string literals are always in scope  - 
%% they carry their own value, not a reference to another system.
is_literal("True") -> true;
is_literal("true") -> true;
is_literal("False") -> true;
is_literal("false") -> true;
is_literal(Token) ->
    case catch list_to_integer(Token) of
        N when is_integer(N) -> true;
        _ ->
            case catch list_to_float(Token) of
                F when is_float(F) -> true;
                _ -> false
            end
    end.

is_a4_builtin(Token) ->
    Types = af_type:all_types(),
    lists:any(fun(T) ->
        case af_type:find_op_by_name(Token, T#af_type.name) of
            {ok, Op} -> not is_user_compiled(Op);
            not_found -> false
        end
    end, Types).

is_user_compiled(#operation{source = {compiled, _}}) -> true;
is_user_compiled(#operation{source = _}) -> false.

%% ---------------------------------------------------------------
%% Utility
%% ---------------------------------------------------------------

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(S) when is_list(S) -> S;
to_list(A) when is_atom(A) -> atom_to_list(A).

format_violation(Path, EventName, Token) ->
    lists:flatten(io_lib:format(
        "axiom_1: system '~s' handler '~s' references '~s' -- "
        "not a parent / child / local / state / transition name "
        "or a4 builtin",
        [Path, EventName, Token])).

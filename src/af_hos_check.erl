-module(af_hos_check).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([check_system/1, check_system_raise/1, init/0]).
-export([register_system/1, lookup_system/1, clear_registry/0,
         registered_child_events/1]).
%% Helpers exposed so the in-progress a4 checker
%% (src/bootstrap/hos/check.a4) can delegate complex graph walks
%% back to BEAM while its own implementation catches up.
-export([a4_unreachable_states/1, a4_initial_state_str/1,
         a4_extra_violations/1,
         a4_emergency_violations/2,
         a4_handler_scope_violations/2,
         a4_handler_transition_violations/2,
         a4_parent_children_violations/1]).
%% Flip of #179: the DSL now calls check_via_a4/1 in place of
%% check_system_raise/1 so a4-side `check-blueprint-a4` owns the
%% axiom checking. The Erlang-side axiom functions remain available
%% as an FFI service to the a4 checker.
-export([check_via_a4/1, ensure_a4_loaded/0]).

-define(REG_KEY, hos_system_registry).
-define(REG_ETS, hos_system_registry_ets).

%% Registry access: now ETS-backed so the system table is visible from
%% every Erlang process (previously process-dict-scoped, which broke
%% once spawn_system delegated to the a4 runtime where child actors
%% want to look up their own sub-systems from the spawned process).
reg_ensure() ->
    case ets:info(?REG_ETS) of
        undefined ->
            ets:new(?REG_ETS, [named_table, set, public]),
            ok;
        _ -> ok
    end.

reg_lookup_raw(NameStr) ->
    reg_ensure(),
    case ets:lookup(?REG_ETS, NameStr) of
        [{_, Sys}] -> {ok, Sys};
        []         -> not_found
    end.

reg_all_entries() ->
    reg_ensure(),
    ets:tab2list(?REG_ETS).

reg_put(NameStr, Sys) ->
    reg_ensure(),
    ets:insert(?REG_ETS, {NameStr, Sys}),
    ok.

reg_clear() ->
    reg_ensure(),
    ets:delete_all_objects(?REG_ETS),
    ok.

%% Axiom checker for HOS HosBlueprint trees.
%%
%% Walks a HosBlueprint tree and verifies Hamilton's control axioms
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
        sig_in = ['HosBlueprint'],
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
register_system({'HosBlueprint', NameBin, _, _, _, _, _} = Sys) ->
    check_parent_children_consistency(Sys),
    Name = binary_to_list(NameBin),
    reg_put(Name, Sys),
    ok.

%% Check parent/children symmetry in BOTH directions against every
%% system currently in the registry. Runs BEFORE this system is
%% added, so any already-registered system whose claims conflict
%% with ours surfaces now.
check_parent_children_consistency({'HosBlueprint', NameBin, ParentNameBin,
                                   _, Children, _, _}) ->
    Name = binary_to_list(NameBin),
    Parent = binary_to_list(ParentNameBin),
    MyChildNames = [binary_to_list(element(2, C)) || C <- Children],
    Reg = maps:from_list(reg_all_entries()),
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
    Key = case Name of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L)   -> L
    end,
    case reg_lookup_raw(Key) of
        {ok, Sys}   -> Sys;
        not_found   -> not_found
    end.

clear_registry() ->
    reg_clear().

%% Given a child name, return the list of event-name strings declared
%% on it. Returns [] if the child isn't in the registry yet (which is
%% legitimate in forward-reference cases  -  the parent's scope just
%% doesn't get the child's events until the child is defined).
registered_child_events(ChildName) ->
    case lookup_system(ChildName) of
        not_found -> [];
        {'HosBlueprint', _, _, _, _, Handlers, _} ->
            [atom_to_list(element(2, H)) || H <- Handlers]
    end.

%% a4-checker bridge: given a transitions list, return the list of
%% state names that are NOT reachable from the initial state (= From
%% of the first transition). Result is a list of binaries so it
%% round-trips through the a4 String type.
a4_unreachable_states([]) -> [];
a4_unreachable_states(Transitions) when is_list(Transitions) ->
    [First | _] = Transitions,
    Initial = atom_to_list(element(2, First)),
    All = all_declared_states(Transitions),
    Reachable = reachable_from(Initial, Transitions, [Initial]),
    [list_to_binary(S) || S <- All, not lists:member(S, Reachable)].

%% a4-checker bridge: initial state of a transitions list, as a
%% binary; returns <<>> when transitions is empty.
a4_initial_state_str([]) -> <<>>;
a4_initial_state_str([First | _]) ->
    list_to_binary(atom_to_list(element(2, First))).

%% Run check-blueprint-a4 through the a4 interpreter and raise if
%% any violation string comes back. Drop-in replacement for
%% check_system_raise/1.
check_via_a4(HosBlueprint) ->
    ensure_a4_loaded(),
    C0 = (af_interpreter:new_continuation())#continuation{
        data_stack = [HosBlueprint]
    },
    C1 = af_interpreter:interpret_token(
        #token{value = "check-blueprint-a4"}, C0),
    case C1#continuation.data_stack of
        [{'List', []}] -> ok;
        [{'List', Items}] ->
            Msgs = [binary_to_list(S) || {'String', S} <- Items],
            case Msgs of
                [] -> ok;
                _ ->
                    Msg = "HOS axiom violations:\n  " ++
                          lists:flatten(lists:join("\n  ", Msgs)),
                    error({axiom_violation, Msg})
            end;
        Other ->
            error({hos_a4_check_unexpected, Other})
    end.

%% Idempotent loader for the two a4 source files the checker needs.
%% runtime.a4 registers HosBlueprint + friends; check.a4 defines
%% check-blueprint-a4. Called at first check_via_a4/1 entry.
%%
%% The load-status probe checks for hos-path on the STOCK 'String'
%% dict. Stock types (Int, String, List, etc.) get their ops table
%% wiped on every af_repl:init_types/0 call (each af_type_*:init
%% re-registers with a fresh ops map), so an op that check.a4 added
%% to the String dict is a reliable signal that the a4 sources were
%% loaded AFTER the most recent init_types. Checking the custom
%% HosBlueprint dict misses this because custom types survive
%% init_types intact and their ops appear "loaded" even after the
%% stock-dict ops they depend on are gone.
ensure_a4_loaded() ->
    case af_type:find_op_by_name("hos-path", 'String') of
        {ok, _} -> ok;
        _ ->
            case persistent_term:get(af_hos_a4_cache, undefined) of
                undefined ->
                    %% First load in this VM: interpret the sources,
                    %% then snapshot the types they touched so future
                    %% reloads can skip the interpreter.
                    load_a4_source("src/bootstrap/hos/runtime.a4"),
                    load_a4_source("src/bootstrap/hos/check.a4"),
                    cache_a4_types();
                Cached ->
                    %% Stock-dict wipe after init_types. Replay the
                    %% cached type snapshots directly — no interpreter,
                    %% no deferred-check queue.
                    replay_a4_cache(Cached)
            end,
            ok
    end.

load_a4_source(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), Path),
            af_interpreter:interpret_tokens(
                Tokens, af_interpreter:new_continuation()),
            ok;
        {error, _} ->
            error({a4_source_missing, Path})
    end.

%% Capture the ops on every type the a4 sources registered against.
%% Stored as [{TypeName, #af_type{}}] in a persistent_term entry so
%% subsequent ensure_a4_loaded/0 calls can replay without going back
%% through the interpreter.
cache_a4_types() ->
    Types = ['Any', 'String', 'List', 'Map', 'Atom', 'Bool', 'Int',
             'Actor', 'HosBlueprint', 'HosSelf', 'HosInit', 'HosCast',
             'HosStop', 'HosCall', 'HosGetLog', 'HosIntrospectChild',
             'HosGetState', 'HandlersWalk', 'TriggersWalk',
             'ReachabilityWalk', 'EffectWalk', 'ChildWalk'],
    Snapshot = [{T, snapshot_type(T)} || T <- Types],
    persistent_term:put(af_hos_a4_cache, Snapshot).

snapshot_type(Name) ->
    case af_type:get_type(Name) of
        {ok, Type} -> Type;
        _ -> undefined
    end.

%% Replay a cached snapshot into ETS. For stock types we only
%% re-add the ops the a4 sources contributed, not the type record
%% itself (init_types already re-registered the type with built-in
%% ops; we'd clobber those if we replaced the whole record).
replay_a4_cache(Cached) ->
    lists:foreach(fun({Name, Type}) -> replay_one(Name, Type) end, Cached).

replay_one(_Name, undefined) -> ok;
replay_one(Name, CachedType) ->
    case af_type:get_type(Name) of
        {ok, CurrentType} ->
            CurrentOps = CurrentType#af_type.ops,
            CachedOps = CachedType#af_type.ops,
            %% Merge: cached ops that aren't in current go into current.
            %% A cached op present in current means the built-in
            %% already re-registered it — leave current alone.
            maps:foreach(fun(OpName, OpList) ->
                case maps:is_key(OpName, CurrentOps) of
                    true -> ok;
                    false ->
                        [af_type:add_op(Name, Op) || Op <- OpList]
                end
            end, CachedOps);
        _ ->
            %% Type isn't registered (custom type wiped); register
            %% the cached record wholesale.
            af_type:register_type(CachedType)
    end.

%% a4-checker bridge: pre-formatted axiom_5 exhaustive-emergency
%% violations for a given transitions list + path. Returns a list of
%% binaries that the a4 checker can wrap as Strings and return.
a4_emergency_violations([], _Path) -> [];
a4_emergency_violations(Transitions, Path)
  when is_list(Transitions), is_binary(Path) ->
    PathStr = binary_to_list(Path),
    ByTrigger = group_by_trigger(Transitions),
    Emerges = [
        {Trigger, SingleTarget}
        || {Trigger, Trans} <- maps:to_list(ByTrigger),
           length(Trans) >= 2,
           [SingleTarget] <- [lists:usort([element(3, T) || T <- Trans])]
    ],
    lists:flatmap(fun({Trigger, Target}) ->
        Msgs = check_one_emergency(Trigger, Target, Transitions, PathStr),
        [list_to_binary(M) || M <- Msgs]
    end, Emerges);
a4_emergency_violations(_, _) -> [].

%% Pre-formatted axiom_1 body-scope violations for every handler
%% in a HosBlueprint. Returns binaries. Accepts the blueprint in
%% either raw-tuple form or in the field-map form af_term produces
%% when a product-type value is passed through erlang-apply.
a4_handler_scope_violations({'HosBlueprint', _, _, _, _, _, _} = BP,
                            Path) when is_binary(Path) ->
    a4_handler_scope_violations_tuple(BP, Path);
a4_handler_scope_violations(#{type := 'HosBlueprint'} = M, Path)
  when is_binary(Path) ->
    a4_handler_scope_violations_tuple(a4_bp_map_to_tuple(M), Path);
a4_handler_scope_violations(_, _) -> [].

a4_handler_scope_violations_tuple({'HosBlueprint', _, ParentRaw,
                                   StateType, Children, Handlers,
                                   Transitions},
                                  Path) ->
    PathStr = binary_to_list(Path),
    Scope = build_scope(PathStr, ParentRaw, StateType, Children,
                        Handlers, Transitions),
    Msgs = lists:flatmap(
             fun(H) -> check_handler(H, Scope, PathStr) end, Handlers),
    [list_to_binary(M) || M <- Msgs].

%% Pre-formatted axiom_5 handler-transition violations.
a4_handler_transition_violations({'HosBlueprint', _, _, _, _, _, _} = BP,
                                 Path) when is_binary(Path) ->
    a4_handler_transition_violations_tuple(BP, Path);
a4_handler_transition_violations(#{type := 'HosBlueprint'} = M, Path)
  when is_binary(Path) ->
    a4_handler_transition_violations_tuple(a4_bp_map_to_tuple(M), Path);
a4_handler_transition_violations(_, _) -> [].

a4_handler_transition_violations_tuple({'HosBlueprint', _, _, _, _,
                                        Handlers, Transitions},
                                       Path) ->
    PathStr = binary_to_list(Path),
    Msgs = lists:flatmap(
             fun(H) -> check_handler_transitions(H, Transitions, PathStr) end,
             Handlers),
    [list_to_binary(M) || M <- Msgs].

%% Undo the field-map conversion af_term:from_stack_item does on
%% product types so our tuple-pattern clauses can match.
a4_bp_map_to_tuple(M) ->
    {'HosBlueprint',
     maps:get(name, M, <<>>),
     maps:get('parent-name', M, <<>>),
     maps:get('state-type', M, 'None'),
     maps:get(children, M, []),
     maps:get(handlers, M, []),
     maps:get(transitions, M, [])}.

%% Pre-formatted parent/children 4-way symmetry violations. Runs the
%% same check_parent_children_consistency logic but returns binaries
%% instead of raising, so the a4 caller can collect and report.
a4_parent_children_violations({'HosBlueprint', _, _, _, _, _, _} = BP) ->
    a4_parent_children_violations_tuple(BP);
a4_parent_children_violations(#{type := 'HosBlueprint'} = M) ->
    a4_parent_children_violations_tuple(a4_bp_map_to_tuple(M));
a4_parent_children_violations(_) -> [].

a4_parent_children_violations_tuple({'HosBlueprint', NameBin, ParentNameBin,
                                     _, Children, _, _}) ->
    Name = binary_to_list(NameBin),
    Parent = binary_to_list(ParentNameBin),
    MyChildNames = [binary_to_list(element(2, C)) || C <- Children],
    Reg = maps:from_list(reg_all_entries()),
    V1 = children_i_declare_check(Name, MyChildNames, Reg),
    V2 = my_parent_check(Name, Parent, Reg),
    V3 = others_who_list_me_check(Name, Parent, Reg),
    V4 = others_who_claim_me_as_parent_check(Name, MyChildNames, Reg),
    [list_to_binary(V) || V <- V1 ++ V2 ++ V3 ++ V4].

%% a4-checker bridge: run every axiom check that the pure-a4 checker
%% has NOT yet re-implemented locally. Takes the same HosBlueprint
%% the a4 checker was handed, returns a list of violation binaries.
%%
%% This covers:
%%   - transition-effect scope (axiom 1 for declared effects)
%%   - exhaustive emergency coverage (axiom 5)
%%   - per-handler body scope walk (axiom 1 for body tokens)
%%   - per-handler transition walk (axiom 5 for -> markers)
%%   - recursive tree walk into children
%%
%% The a4-owned axioms (stateful-body, trigger-scope, state-
%% reachability) are EXCLUDED here so check-blueprint-a4 does not
%% double-count them.
a4_extra_violations({'HosBlueprint', _, _, _, _, _, _} = Sys) ->
    a4_extra_violations_tuple(Sys);
a4_extra_violations(#{type := 'HosBlueprint'} = M) ->
    a4_extra_violations_tuple(a4_bp_map_to_tuple(M));
a4_extra_violations(_) -> [].

a4_extra_violations_tuple(Sys) ->
    All = violations(Sys),
    Owned = a4_owned_violation_prefixes(),
    Remaining = [V || V <- All, not starts_with_any(V, Owned)],
    [list_to_binary(V) || V <- Remaining].

a4_owned_violation_prefixes() ->
    %% Violations produced by the pure-a4 checker. Used to filter
    %% them out of the Erlang side's output so the two don't double
    %% up when check-blueprint-a4 appends both.
    %%
    %% Match by the structural shape of the message, not by its
    %% exact text, so wording changes on either side don't create
    %% accidental duplicates. Each predicate returns true iff the
    %% violation belongs to an a4-owned axiom.
    %% Only the axioms check-blueprint-a4 actually runs inside its
    %% own body are filtered out here. Other a4-wrapper words
    %% (check-transition-effects, check-emergency-coverage,
    %% check-handler-scope, check-handler-transitions) exist as
    %% callable wrappers but aren't wired into the top driver, so
    %% the Erlang bridge must still produce those violations for
    %% parity with the old path.
    [fun stateful_body_violation/1,
     fun trigger_scope_violation/1,
     fun state_reachability_violation/1].

stateful_body_violation(Msg) ->
    %% "axiom_5: system '...' handler '...' has a non-empty body; ..."
    has_substring(Msg, "has a non-empty body").

trigger_scope_violation(Msg) ->
    %% "axiom_1: system '...' transition X -> Y uses trigger '...'
    %%  but '...' is not a declared handler..."
    %% Tightened so it doesn't swallow the timer-effect violation
    %% (which also says "is not a declared handler..." but starts
    %% with "declares timer effect").
    has_substring(Msg, "axiom_1")
        andalso has_substring(Msg, "uses trigger")
        andalso has_substring(Msg, "is not a declared handler on this system").

state_reachability_violation(Msg) ->
    %% "axiom_5: system '...' state '...' is unreachable from initial
    %% state '...'. Either declare a transition into '...' or delete it."
    has_substring(Msg, "is unreachable from initial state").

transition_effect_violation(Msg) ->
    %% Two shapes from check.a4 for axiom_1 transition-effect target:
    %%  - timer: "...timer effect schedules event '...' but '...' is
    %%    not a declared handler..."
    %%  - non-timer: "...transition effect target '...' is not in scope"
    has_substring(Msg, "axiom_1")
        andalso (has_substring(Msg, "timer effect schedules event")
                 orelse has_substring(Msg, "transition effect target")
                 orelse has_substring(Msg, "with effect on")).

exhaustive_emergency_violation(Msg) ->
    %% "axiom_5: system '...' has emergency trigger '...' converging
    %% on state '...', but state '...' has no declared transition
    %% under '...'."
    has_substring(Msg, "has emergency trigger").

handler_scope_violation(Msg) ->
    %% "axiom_1: system '<path>' handler '<event>' references '<token>'
    %% -- not a parent / child / local / state / transition name or a4
    %% builtin"
    has_substring(Msg, "axiom_1")
        andalso has_substring(Msg, "handler")
        andalso has_substring(Msg, "references").

handler_transition_violation(Msg) ->
    %% Two shapes from check_handler_transitions:
    %%  - entry: "handler '<event>' starts with transition to '<target>'
    %%    but no declared entry state ..."
    %%  - step:  "handler '<event>' attempts undeclared transition
    %%    <from> -> <to>"
    has_substring(Msg, "axiom_5")
        andalso (has_substring(Msg, "starts with transition")
                 orelse has_substring(Msg, "attempts undeclared transition")).

starts_with_any(Msg, Preds) ->
    lists:any(fun(P) -> P(Msg) end, Preds).

has_substring(String, Needle) ->
    string:find(String, Needle) =/= nomatch.

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

walk({'HosBlueprint', NameRaw, ParentRaw, StateType,
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
    TransitionV  = check_transition_effects(Transitions, Scope, Handlers, Path),
    TriggerV     = check_transition_triggers(Transitions, Handlers,
                                              ParentStr, ChildNames, Path),
    ReachabilityV = check_state_reachability(Transitions, StateType, Path),
    EmergencyV   = check_exhaustive_emergencies(Transitions, StateType, Path),
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
    TransitionV ++ TriggerV ++ ReachabilityV ++ EmergencyV
        ++ HandlerV ++ ChildV.

%% Axiom 1 for declared effects: effect_target (if present) must be
%% in the system's scope. A transition that invokes a subsystem name
%% the system cannot see is an out-of-scope reference just like a
%% body token would be.
%%
%% The reserved target name "after" indicates a timer primitive
%% (scheduled self-send). For "after" effects we verify the event
%% is a declared handler on this system: a timer that fires an
%% event nobody handles would be silently useless.
check_transition_effects(Transitions, Scope, Handlers, Path) ->
    lists:flatmap(fun(T) -> check_transition_effect(T, Scope, Handlers, Path) end,
                  Transitions).

%% 7-tuple (current): with effect_delay.
check_transition_effect({'TransitionSpec', From, To, Trigger,
                         EffectTarget, EffectEvent, _Delay},
                        Scope, Handlers, Path) ->
    check_effect_target(From, To, Trigger, EffectTarget, EffectEvent,
                        Scope, Handlers, Path);
%% 6-tuple (legacy): no effect_delay.
check_transition_effect({'TransitionSpec', From, To, Trigger,
                         EffectTarget, EffectEvent},
                        Scope, Handlers, Path) ->
    check_effect_target(From, To, Trigger, EffectTarget, EffectEvent,
                        Scope, Handlers, Path);
%% 4-tuple (legacy): no effect at all.
check_transition_effect({'TransitionSpec', _, _, _}, _, _, _) ->
    [].

check_effect_target(From, To, Trigger, EffectTarget, EffectEvent,
                    Scope, Handlers, Path) ->
    case effect_target_str(EffectTarget) of
        "" -> [];
        "after" ->
            %% Timer primitive: the scheduled event must be a
            %% handler declared on this system, otherwise nothing
            %% receives the self-send.
            EventStr = atom_to_list(EffectEvent),
            OwnEvents = [atom_to_list(element(2, H)) || H <- Handlers],
            case lists:member(EventStr, OwnEvents) of
                true  -> [];
                false ->
                    [lists:flatten(io_lib:format(
                        "axiom_1: system '~s' declares timer effect "
                        "~s -> ~s under trigger '~s' scheduling "
                        "event '~s', but '~s' is not a declared "
                        "handler on this system",
                        [Path, From, To, Trigger,
                         EventStr, EventStr]))]
            end;
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
    end.

effect_target_str(<<>>) -> "";
effect_target_str("") -> "";
effect_target_str(B) when is_binary(B) -> binary_to_list(B);
effect_target_str(L) when is_list(L) -> L.

%% Stateful systems are driven by the transitions table. Their
%% `on X -> ;` declarations are optional (documentation of expected
%% events) but must have EMPTY bodies. Any body content is an axiom
%% violation: a stateful handler body cannot declare state markers
%% (the table declares them), cannot contain subsystem calls (those
%% are transition effects), and cannot contain arbitrary code
%% (there is no data-flow surface). One event fires at most one
%% transition, chosen by the table from the live state. Sequences
%% of transitions happen because multiple events arrive, each one
%% driving one step.
check_stateful_body(_Handler, 'None', _Parent, _Children, _CEvents, _Path) ->
    [];
check_stateful_body({'HandlerSpec', _EventName, _SigIn, _SigOut, []},
                    _StateType, _ParentStr, _ChildNames, _ChildEvents, _Path) ->
    [];
check_stateful_body({'HandlerSpec', EventName, _SigIn, _SigOut, _Body},
                    _StateType, _ParentStr, _ChildNames, _ChildEvents, Path) ->
    EventStr = atom_to_list(EventName),
    [lists:flatten(io_lib:format(
        "axiom_5: system '~s' handler '~s' has a non-empty body; "
        "stateful systems are driven by the transitions table "
        "(one event fires one transition). Delete the body and "
        "declare transitions for this trigger instead.",
        [Path, EventStr]))].

path("", Name) -> Name;
path(Prefix, Name) -> Prefix ++ "." ++ Name.

%% ---------------------------------------------------------------
%% Tier 1: trigger scope check.
%%
%% A transition's trigger names an event this system will receive.
%% The trigger must be a declared `on X -> ;` handler on THIS system.
%% Upward signals from children and commands from a parent are both
%% received here, so both are modelled as declared handlers. A trigger
%% that is not declared is almost always a typo: the runtime will
%% silently input-reject it (Axiom 4 safety), producing a no-op that
%% the author will struggle to diagnose. Raising at compile time
%% surfaces the typo immediately.
%% ---------------------------------------------------------------

check_transition_triggers(Transitions, Handlers, _ParentStr, _ChildNames, Path) ->
    HandlerNames = [atom_to_list(element(2, H)) || H <- Handlers],
    lists:flatmap(fun(T) ->
        Trigger = atom_to_list(element(4, T)),
        case lists:member(Trigger, HandlerNames) of
            true -> [];
            false ->
                [lists:flatten(io_lib:format(
                    "axiom_1: system '~s' transition ~s -> ~s uses "
                    "trigger '~s' but '~s' is not a declared handler "
                    "on this system. Add `on ~s -> ;` to accept this "
                    "event.",
                    [Path, element(2, T), element(3, T),
                     Trigger, Trigger, Trigger]))]
        end
    end, Transitions).

%% ---------------------------------------------------------------
%% Tier 1: state reachability check.
%%
%% Every state named in the transitions table (as a From or a To)
%% must be reachable from the initial state by following some chain
%% of transitions. Unreachable states are dead code: they clutter
%% the spec, mislead a reader, and usually signal a typo (wrong
%% state name) or stale state after a refactor.
%%
%% The initial state is the From of the first declared transition,
%% matching the runtime's choice in af_hos_runtime:initial_state/1.
%% Stateless systems have no state graph and are skipped.
%% ---------------------------------------------------------------

check_state_reachability(_Transitions, 'None', _Path) -> [];
check_state_reachability([], _StateType, _Path) -> [];
check_state_reachability([First | _] = Transitions, _StateType, Path) ->
    Initial = atom_to_list(element(2, First)),
    AllStates = all_declared_states(Transitions),
    Reachable = reachable_from(Initial, Transitions, [Initial]),
    Unreachable = [S || S <- AllStates, not lists:member(S, Reachable)],
    lists:map(fun(S) ->
        lists:flatten(io_lib:format(
            "axiom_5: system '~s' state '~s' is unreachable from "
            "initial state '~s'. Either declare a transition into "
            "'~s' or delete it.",
            [Path, S, Initial, S]))
    end, Unreachable).

all_declared_states(Transitions) ->
    Froms = [atom_to_list(element(2, T)) || T <- Transitions],
    Tos   = [atom_to_list(element(3, T)) || T <- Transitions],
    lists:usort(Froms ++ Tos).

reachable_from(State, Transitions, Visited) ->
    Outgoing = [atom_to_list(element(3, T)) || T <- Transitions,
                                                atom_to_list(element(2, T))
                                                    =:= State],
    NewStates = [S || S <- Outgoing, not lists:member(S, Visited)],
    case NewStates of
        [] -> Visited;
        _ ->
            Visited2 = Visited ++ NewStates,
            lists:foldl(fun(S, Acc) ->
                reachable_from(S, Transitions, Acc)
            end, Visited2, NewStates)
    end.

%% ---------------------------------------------------------------
%% Tier 1: exhaustive emergency-entry check.
%%
%% An "emergency trigger" is a trigger under which two or more
%% transitions converge on a single target state. That target is
%% the emergency entry state. Every state of the system that is
%% neither the emergency target itself nor downstream in the
%% recovery path from the emergency target (reached via some non-
%% emergency trigger chain that does not loop back to a state with
%% the emergency trigger declared) MUST have a transition under the
%% emergency trigger into the emergency target. Otherwise an
%% emergency arriving while the system is in that non-covered state
%% would be silently input-rejected (Axiom 4) and the emergency
%% would be missed.
%%
%% Rationale: the correctness argument for emergency preemption
%% rests on the BEAM mailbox serializing messages per actor and
%% Axiom 4 input rejection discarding stale scheduled self-sends.
%% Those two properties only deliver a preemption guarantee if
%% every live state declares a transition into the emergency state.
%% Miss one and the actor just ignores the fire alarm.
%% ---------------------------------------------------------------

check_exhaustive_emergencies(_Transitions, 'None', _Path) -> [];
check_exhaustive_emergencies(Transitions, _StateType, Path) ->
    ByTrigger = group_by_trigger(Transitions),
    EmergencyTriggers = [
        {Trigger, SingleTarget}
        || {Trigger, Trans} <- maps:to_list(ByTrigger),
           length(Trans) >= 2,
           [SingleTarget] <- [lists:usort([element(3, T) || T <- Trans])]
    ],
    lists:flatmap(fun({Trigger, Target}) ->
        check_one_emergency(Trigger, Target, Transitions, Path)
    end, EmergencyTriggers).

group_by_trigger(Transitions) ->
    lists:foldl(fun(T, Acc) ->
        Trig = element(4, T),
        Acc#{Trig => [T | maps:get(Trig, Acc, [])]}
    end, #{}, Transitions).

check_one_emergency(Trigger, TargetAtom, Transitions, Path) ->
    Target = atom_to_list(TargetAtom),
    TriggerStr = atom_to_list(Trigger),
    AllStates = all_declared_states(Transitions),
    RecoveryReachable = recovery_states(Target, Trigger, Transitions),
    Required = [S || S <- AllStates,
                     S =/= Target,
                     not lists:member(S, RecoveryReachable)],
    Covered = [atom_to_list(element(2, T)) || T <- Transitions,
                                               element(4, T) =:= Trigger],
    Missing = [S || S <- Required, not lists:member(S, Covered)],
    lists:map(fun(S) ->
        lists:flatten(io_lib:format(
            "axiom_5: system '~s' has emergency trigger '~s' "
            "converging on state '~s', but state '~s' has no "
            "declared transition under '~s'. An emergency "
            "arriving in '~s' would be silently dropped.",
            [Path, TriggerStr, Target, S, TriggerStr, S]))
    end, Missing).

%% States reachable from the emergency target via non-emergency
%% transitions, stopping at any state that has the emergency trigger
%% declared (those are main-graph states reached through a recovery
%% edge, and they still need their own emergency coverage).
recovery_states(Target, EmergencyTrigger, Transitions) ->
    recovery_states_walk([Target], EmergencyTrigger, Transitions, [Target]).

recovery_states_walk([], _Trigger, _Transitions, Visited) -> Visited;
recovery_states_walk([State | Queue], Trigger, Transitions, Visited) ->
    Outgoing = [atom_to_list(element(3, T)) || T <- Transitions,
                                                atom_to_list(element(2, T))
                                                    =:= State,
                                                element(4, T) =/= Trigger],
    New = [S || S <- Outgoing,
                not lists:member(S, Visited),
                not has_trigger_declared(S, Trigger, Transitions)],
    recovery_states_walk(Queue ++ New, Trigger, Transitions, Visited ++ New).

has_trigger_declared(State, Trigger, Transitions) ->
    lists:any(fun(T) ->
        atom_to_list(element(2, T)) =:= State andalso element(4, T) =:= Trigger
    end, Transitions).

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

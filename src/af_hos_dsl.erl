-module(af_hos_dsl).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% HOS `system ... end` DSL surface.
%%
%% Grammar (domain-first, not a4-flavored):
%%
%%   system Name
%%       parent ParentName
%%       state  StateType
%%       children Child1 Child2 ...
%%
%%       on EventName SigIn1 SigIn2 -> SigOut1 ;
%%           body tokens ...
%%
%%       on OtherEvent -> ;
%%           body tokens ...
%%
%%       transitions
%%           FromState -> ToState Trigger
%%           FromState2 -> ToState2 Trigger2
%%   end
%%
%% The `system` keyword pushes a SystemDef collection state onto the
%% data stack. All subsequent tokens route through handle_system_def/2
%% (via the SystemDef type's handler) and accumulate as raw
%% {Value, IsQuoted} pairs. `end` finalises the collection  -  it
%% unambiguously closes the system definition, leaving `;` free as
%% the handler-header terminator. The accumulated tokens are parsed
%% into a HosBlueprint product instance and the axiom checker runs
%% eagerly  -  a spec that violates Axiom 1 raises immediately.

init() ->
    af_type:register_type(#af_type{
        name = 'SystemDef',
        ops = #{},
        handler = fun handle_system_def/2
    }),
    af_type:add_op('Any', #operation{
        name = "system",
        sig_in = [],
        sig_out = ['SystemDef'],
        impl = fun op_system_start/1
    }),
    af_type:add_op('Any', #operation{
        name = "find-system",
        sig_in = ['String'],
        sig_out = ['HosBlueprint'],
        impl = fun op_find_system/1
    }).


%% ---------------------------------------------------------------
%% Entry: `system` pushes the collection state.
%% ---------------------------------------------------------------

op_system_start(Cont) ->
    State = #{tokens => []},
    Cont#continuation{
        data_stack = [{'SystemDef', State} | Cont#continuation.data_stack]
    }.

%% find-system "Name" -> HosBlueprint. Pulls a previously-defined
%% HosBlueprint out of the registry. Raises if no such system is
%% defined.
op_find_system(Cont) ->
    [{'String', NameBin} | Rest] = Cont#continuation.data_stack,
    Name = binary_to_list(NameBin),
    case af_hos_check:lookup_system(Name) of
        not_found ->
            af_error:raise(system_not_found,
                "No HOS system named '" ++ Name ++ "' is defined.",
                Cont);
        HosBlueprint ->
            Cont#continuation{data_stack = [HosBlueprint | Rest]}
    end.


%% ---------------------------------------------------------------
%% Token interceptor: every token between `system` and `;` routes
%% here. `;` finalises the collection and produces a HosBlueprint.
%% ---------------------------------------------------------------

handle_system_def("end", Cont) ->
    [{'SystemDef', State} | Rest] = Cont#continuation.data_stack,
    #{tokens := RevTokens} = State,
    Tokens = lists:reverse(RevTokens),
    HosBlueprint = parse_system_tokens(Tokens),
    %% Check axioms eagerly. a4 checker (check_via_a4/1) is wired
    %% and functionally correct; the flip is pending a cheaper
    %% load path because the a4-source reload after every
    %% af_repl:init_types/0 blows eunit per-test timeouts. See #179.
    af_hos_check:check_system_raise(HosBlueprint),
    %% Register so later systems can look up this one's events when
    %% building their scope (parent-to-child event invocation).
    af_hos_check:register_system(HosBlueprint),
    %% Do NOT push the HosBlueprint back onto the stack. `end` is a
    %% definition boundary, not a value producer. Callers that need
    %% the value should use `find-system "Name"` to pull it from the
    %% registry, which keeps the sample code free of trailing `drop`s.
    Cont#continuation{data_stack = Rest};
handle_system_def(TokenValue, Cont) ->
    [{'SystemDef', State} | Rest] = Cont#continuation.data_stack,
    #{tokens := RevTokens} = State,
    IsQuoted = case Cont#continuation.current_token of
        #token{quoted = Q} -> Q;
        _ -> false
    end,
    NewState = State#{tokens => [{TokenValue, IsQuoted} | RevTokens]},
    Cont#continuation{data_stack = [{'SystemDef', NewState} | Rest]}.


%% ---------------------------------------------------------------
%% Token-stream parser
%% ---------------------------------------------------------------
%% Input: list of {Value, IsQuoted} in source order. First token is
%% the system name; the rest is a sequence of section keywords and
%% their arguments.

parse_system_tokens([{Name, _} | Rest]) ->
    Acc0 = #{
        name => Name,
        parent_name => "",
        state_type => 'None',
        children => [],
        handlers => [],
        transitions => []
    },
    Acc = parse_sections(Rest, Acc0),
    build_system_node(Acc);
parse_system_tokens([]) ->
    error({hos_dsl, "`system` requires a name before any section"}).

parse_sections([], Acc) ->
    Acc;
parse_sections([{"parent", _}, {ParentName, _} | Rest], Acc) ->
    parse_sections(Rest, Acc#{parent_name => ParentName});
parse_sections([{"state", _}, {StateType, _} | Rest], Acc) ->
    parse_sections(Rest, Acc#{state_type => list_to_atom(StateType)});
parse_sections([{"children", _} | Rest], Acc) ->
    {ChildNames, Rest2} = collect_until_section(Rest),
    parse_sections(Rest2, Acc#{children => ChildNames});
parse_sections([{"on", _} | Rest], Acc) ->
    {Handler, Rest2} = parse_on_handler(Rest),
    Handlers = maps:get(handlers, Acc),
    parse_sections(Rest2, Acc#{handlers => Handlers ++ [Handler]});
parse_sections([{"transitions", _} | Rest], Acc) ->
    {Transitions, Rest2} = parse_transitions(Rest),
    Ts = maps:get(transitions, Acc),
    parse_sections(Rest2, Acc#{transitions => Ts ++ Transitions});
parse_sections([{Other, _} | _], _Acc) ->
    error({hos_dsl, lists:flatten(
        io_lib:format("unexpected token '~s' where a section keyword "
                      "(parent / state / children / on / transitions) "
                      "was expected", [Other]))}).


%% ---------------------------------------------------------------
%% Section sub-parsers
%% ---------------------------------------------------------------

is_section_keyword("parent")      -> true;
is_section_keyword("state")       -> true;
is_section_keyword("children")    -> true;
is_section_keyword("on")          -> true;
is_section_keyword("transitions") -> true;
is_section_keyword(_)             -> false.

collect_until_section(Tokens) ->
    collect_until_section(Tokens, []).

collect_until_section([], Acc) ->
    {lists:reverse(Acc), []};
collect_until_section([{Val, _} | _] = Toks, Acc) ->
    case is_section_keyword(Val) of
        true  -> {lists:reverse(Acc), Toks};
        false -> collect_until_section(tl(Toks), [Val | Acc])
    end.

%% `on Event InType1 InType2 -> OutType1 ;  body... `
%% Body ends at the next section keyword or end-of-input.
parse_on_handler([{EventName, _} | Rest]) ->
    {SigInRaw, AfterSigIn} = collect_sig(Rest),
    AfterArrow = expect_token("->", AfterSigIn, "in `on` handler header"),
    {SigOutRaw, AfterSigOut} = collect_sig(AfterArrow),
    AfterSemi = expect_token(";", AfterSigOut, "in `on` handler header"),
    {BodyRaw, Rest2} = collect_until_section(AfterSemi),
    Handler =
        {'HandlerSpec',
         list_to_atom(EventName),
         [list_to_atom(T) || T <- SigInRaw],
         [list_to_atom(T) || T <- SigOutRaw],
         [{'String', list_to_binary(T)} || T <- BodyRaw]},
    {Handler, Rest2};
parse_on_handler([]) ->
    error({hos_dsl, "`on` requires an event name"}).

%% Collect signature tokens up to `->` or `;`.
collect_sig(Tokens) -> collect_sig(Tokens, []).
collect_sig([{"->", _} | _] = Toks, Acc) -> {lists:reverse(Acc), Toks};
collect_sig([{";",  _} | _] = Toks, Acc) -> {lists:reverse(Acc), Toks};
collect_sig([{T, _} | Rest], Acc) -> collect_sig(Rest, [T | Acc]);
collect_sig([], Acc) -> {lists:reverse(Acc), []}.

expect_token(Expected, [{Expected, _} | Rest], _Where) ->
    Rest;
expect_token(Expected, [{Got, _} | _], Where) ->
    error({hos_dsl, lists:flatten(
        io_lib:format("expected '~s' ~s but got '~s'",
                      [Expected, Where, Got]))});
expect_token(Expected, [], Where) ->
    error({hos_dsl, lists:flatten(
        io_lib:format("expected '~s' ~s but reached end of input",
                      [Expected, Where]))}).

%% Transitions section. Each entry is:
%%     From -> To Trigger
%%     From -> To Trigger : Target event
%%     From -> To Trigger : after <ms> event
%%
%% The `: Target event` clause declares the subsystem interaction
%% that happens when this transition fires. Because the effect is in
%% the table rather than hidden in a handler body, any reordering or
%% wrong target is visible in the declared transitions. A stateful
%% system's handler body can therefore contain only state markers;
%% subsystem calls live only on transitions (enforced in af_hos_check).
%%
%% The `: after <ms> event` variant is the timer primitive: schedule
%% a self-send of `event` after <ms> milliseconds. The mailbox stays
%% open during the wait; preemption is free via Axiom 4 input
%% rejection of any late event whose transition is not declared from
%% the live state.
parse_transitions(Tokens) ->
    parse_transitions(Tokens, []).

parse_transitions([], Acc) ->
    {lists:reverse(Acc), []};
parse_transitions([{Val, _} | _] = Toks, Acc) ->
    case is_section_keyword(Val) of
        true -> {lists:reverse(Acc), Toks};
        false ->
            [{From, _}, ArrowPair, {To, _}, {Trigger, _} | Rest0] = Toks,
            "->" = element(1, ArrowPair),
            {EffectTarget, EffectEvent, EffectDelay, Rest} =
                parse_effect_clause(Rest0),
            T = {'TransitionSpec',
                 list_to_atom(From),
                 list_to_atom(To),
                 list_to_atom(Trigger),
                 EffectTarget,
                 EffectEvent,
                 EffectDelay},
            parse_transitions(Rest, [T | Acc])
    end.

%% Effect clause forms:
%%   (absent)                  -> no effect.
%%   : Target event            -> immediate subsystem dispatch.
%%   : after <ms> event        -> scheduled self-send.
%% Returns {TargetBinary, EventAtom, DelayMs, Rest}.
parse_effect_clause([{":", _}, {"after", _}, {DelayStr, _}, {Event, _} | Rest]) ->
    Delay = list_to_integer(DelayStr),
    {<<"after">>, list_to_atom(Event), Delay, Rest};
parse_effect_clause([{":", _}, {Target, _}, {Event, _} | Rest]) ->
    {list_to_binary(Target), list_to_atom(Event), 0, Rest};
parse_effect_clause(Rest) ->
    {<<>>, 'none', 0, Rest}.


%% ---------------------------------------------------------------
%% Final HosBlueprint construction
%% ---------------------------------------------------------------

build_system_node(Acc) ->
    #{name := Name, parent_name := Parent, state_type := StateType,
      children := ChildNames, handlers := Handlers,
      transitions := Transitions} = Acc,
    %% Children declared in a system are NAME references. The actual
    %% child systems are defined elsewhere; at check time we just need
    %% the names in scope. Represent each as a stub HosBlueprint so the
    %% checker's tree-walk skips them cleanly.
    Children = [
        {'HosBlueprint', list_to_binary(Cn), list_to_binary(Name),
         'None', [], [], []}
        || Cn <- ChildNames
    ],
    {'HosBlueprint',
     list_to_binary(Name),
     list_to_binary(Parent),
     StateType,
     Children,
     Handlers,
     Transitions}.

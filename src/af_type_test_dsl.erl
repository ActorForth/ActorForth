-module(af_type_test_dsl).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Test DSL — scope management + body capture.
%%
%% Syntax:
%%
%%   list_ops group                               ← atom: short identifier
%%       "cons chains build list" test :         ← string: descriptive name
%%           ...
%%       ;
%%       "drop on empty raises" test-raises stack_underflow :
%%           ...
%%       ;
%%       setup :
%%           ... setup body ...
%%       ;
%%       teardown :
%%           ... teardown body ...
%%       ;
%%   .
%%
%% Groups close with `.` — same closer the compiler uses for word
%% definitions. Multi-clause dispatch on TOS type resolves the three
%% meanings of `.`:
%%   TOS = CodeCompile   → compiler's end-word-definition
%%   TOS = GroupScope    → this module's close-group
%%   TOS = anything else → Any-level `.` (print TOS)
%%
%% Group names are Atoms (Forth-style identifiers); test names are
%% Strings (natural-language descriptions). Both sigs are strict — no
%% 'Any' or coercion.
%%
%% Storage:
%%   - GroupScope sentinels live on data_stack during capture; nested
%%     groups produce a stack of sentinels from innermost (TOS) to
%%     outermost. `test`/`setup`/`teardown` stack their own sentinels
%%     above whatever GroupScope is on top; `;` / `.` pops accordingly.
%%   - test_registry accumulates test specs:
%%       #{group_path => [...], name => ..., kind => positive | {raises, Atom},
%%         body => [#token{}...], setup => [...], teardown => [...],
%%         location => {File, Line}}
%%
%% ETS policy: only init-time op registration touches ETS. All
%% registration state lives on the continuation, so parallel test
%% actors cannot race.

init() ->
    af_type:register_type(#af_type{name = 'GroupScope'}),
    af_type:register_type(#af_type{name = 'TestPending'}),
    af_type:register_type(#af_type{name = 'TestRaisesPending'}),
    af_type:register_type(#af_type{name = 'TestBody'}),
    af_type:register_type(#af_type{name = 'TestRaisesBody'}),
    af_type:register_type(#af_type{name = 'SetupBody'}),
    af_type:register_type(#af_type{name = 'TeardownBody'}),
    af_type:register_type(#af_type{name = 'SkipMarker'}),

    %% Entry words on Any. Strict sigs: group takes an Atom identifier;
    %% test / test-raises take a String description.
    af_type:add_op('Any', #operation{
        name = "group", sig_in = ['Atom'], sig_out = ['GroupScope'],
        impl = fun op_group/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "group-serial", sig_in = ['Atom'], sig_out = ['GroupScope'],
        impl = fun op_group_serial/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "test", sig_in = ['String'], sig_out = ['TestPending'],
        impl = fun op_test/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "test-raises", sig_in = ['String'], sig_out = ['TestRaisesPending'],
        impl = fun op_test_raises/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "setup", sig_in = [], sig_out = ['SetupBody'],
        impl = fun op_setup/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "teardown", sig_in = [], sig_out = ['TeardownBody'],
        impl = fun op_teardown/1, source = af_type_test_dsl
    }),

    %% `"name" "reason" skip-test` — register as skipped, don't run body.
    %% Mirrors the `"name" ... test` / `"name" test-raises ...` rhythm.
    af_type:add_op('Any', #operation{
        name = "skip-test", sig_in = ['String', 'String'], sig_out = ['TestPending'],
        impl = fun op_skip_test/1, source = af_type_test_dsl
    }),

    %% Depth assertions attached to the current GroupScope frame. N is
    %% applied to every test registered within this group. Registered on
    %% 'Int' because TOS is the N literal when the word fires; the op's
    %% sig_in requires GroupScope directly below.
    af_type:add_op('Int', #operation{
        name = "max-depth", sig_in = ['Int', 'GroupScope'], sig_out = ['GroupScope'],
        impl = fun op_max_depth/1, source = af_type_test_dsl
    }),
    af_type:add_op('Int', #operation{
        name = "max-return-depth", sig_in = ['Int', 'GroupScope'], sig_out = ['GroupScope'],
        impl = fun op_max_return_depth/1, source = af_type_test_dsl
    }),

    %% `.` on GroupScope closes the group. Multi-clause with the
    %% compiler's `.` on CodeCompile and the Any-level print-TOS `.`.
    af_type:add_op('GroupScope', #operation{
        name = ".", sig_in = ['GroupScope'], sig_out = [],
        impl = fun op_close_group/1, source = af_type_test_dsl
    }),

    %% Transition ops (`:`) pushing the corresponding body type.
    af_type:add_op('TestPending', #operation{
        name = ":", sig_in = ['TestPending'], sig_out = ['TestBody'],
        impl = fun op_test_colon/1, source = af_type_test_dsl
    }),
    af_type:add_op('TestRaisesPending', #operation{
        name = ":", sig_in = ['TestRaisesPending'], sig_out = ['TestRaisesBody'],
        impl = fun op_test_raises_colon/1, source = af_type_test_dsl
    }),

    %% Closers (`;`) for each body type.
    af_type:add_op('TestBody', #operation{
        name = ";", sig_in = ['TestBody'], sig_out = [],
        impl = fun op_close_test/1, source = af_type_test_dsl
    }),
    af_type:add_op('TestRaisesBody', #operation{
        name = ";", sig_in = ['TestRaisesBody'], sig_out = [],
        impl = fun op_close_test_raises/1, source = af_type_test_dsl
    }),
    af_type:add_op('SetupBody', #operation{
        name = ";", sig_in = ['SetupBody'], sig_out = [],
        impl = fun op_close_setup/1, source = af_type_test_dsl
    }),
    af_type:add_op('TeardownBody', #operation{
        name = ";", sig_in = ['TeardownBody'], sig_out = [],
        impl = fun op_close_teardown/1, source = af_type_test_dsl
    }),

    %% Handlers for Pending states (expect an immediate transition) and
    %% the shared body-capture handler.
    register_with_handler('TestPending',       fun handle_test_pending/2),
    register_with_handler('TestRaisesPending', fun handle_test_raises_pending/2),
    register_with_handler('TestBody',          fun handle_body_capture/2),
    register_with_handler('TestRaisesBody',    fun handle_body_capture/2),
    register_with_handler('SetupBody',         fun handle_body_capture/2),
    register_with_handler('TeardownBody',      fun handle_body_capture/2),

    ok.

register_with_handler(TypeName, Handler) ->
    Ops = case af_type:get_type(TypeName) of
        {ok, #af_type{ops = O}} -> O;
        _ -> #{}
    end,
    af_type:register_type(#af_type{
        name    = TypeName,
        ops     = Ops,
        handler = Handler
    }).

%%% Entry words

op_group(#continuation{data_stack = [{'Atom', Name} | Rest]} = Cont) ->
    %% Store the name as a binary so later code (spec lookups, reporting)
    %% sees the same shape as the previous String-based implementation.
    NameBin = list_to_binary(Name),
    Frame = {'GroupScope', #{name => NameBin, setup => [], teardown => [],
                             serial => false}},
    Cont#continuation{data_stack = [Frame | Rest]}.

op_group_serial(#continuation{data_stack = [{'Atom', Name} | Rest]} = Cont) ->
    NameBin = list_to_binary(Name),
    Frame = {'GroupScope', #{name => NameBin, setup => [], teardown => [],
                             serial => true}},
    Cont#continuation{data_stack = [Frame | Rest]}.

op_close_group(#continuation{data_stack = [{'GroupScope', _} | Rest]} = Cont) ->
    Cont#continuation{data_stack = Rest}.

op_test(#continuation{data_stack = [{'String', Name} | Rest]} = Cont) ->
    Cont#continuation{
        data_stack = [{'TestPending', #{name => Name}} | Rest]
    }.

op_test_raises(#continuation{data_stack = [{'String', Name} | Rest]} = Cont) ->
    Cont#continuation{
        data_stack = [{'TestRaisesPending', #{name => Name, err_type => undefined}} | Rest]
    }.

op_setup(#continuation{data_stack = DS} = Cont) ->
    Cont#continuation{
        data_stack = [{'SetupBody', #{body => []}} | DS]
    }.

op_teardown(#continuation{data_stack = DS} = Cont) ->
    Cont#continuation{
        data_stack = [{'TeardownBody', #{body => []}} | DS]
    }.

op_skip_test(#continuation{data_stack = [{'String', Reason}, {'String', Name} | Rest]} = Cont) ->
    Cont#continuation{
        data_stack = [{'TestPending', #{name => Name, skip => Reason}} | Rest]
    }.

%% `max-depth N` on GroupScope: consume Int + GroupScope, attach limit.
op_max_depth(#continuation{data_stack = [{'Int', N}, {'GroupScope', F} | Rest]} = Cont) ->
    Cont#continuation{data_stack = [{'GroupScope', F#{max_depth => N}} | Rest]}.

op_max_return_depth(#continuation{data_stack = [{'Int', N}, {'GroupScope', F} | Rest]} = Cont) ->
    Cont#continuation{data_stack = [{'GroupScope', F#{max_return_depth => N}} | Rest]}.

%%% Transitions (`:`)

op_test_colon(#continuation{data_stack = [{'TestPending', State} | Rest],
                            current_token = Tok} = Cont) ->
    Body = #{name     => maps:get(name, State),
             body     => [],
             location => token_location(Tok),
             skip     => maps:get(skip, State, undefined)},
    Cont#continuation{data_stack = [{'TestBody', Body} | Rest]}.

op_test_raises_colon(#continuation{data_stack = [{'TestRaisesPending', State} | Rest],
                                   current_token = Tok} = Cont) ->
    ErrType = maps:get(err_type, State),
    case ErrType of
        undefined ->
            erlang:error({test_raises, missing_err_type});
        _ ->
            Body = #{name     => maps:get(name, State),
                     err_type => ErrType,
                     body     => [],
                     location => token_location(Tok)},
            Cont#continuation{data_stack = [{'TestRaisesBody', Body} | Rest]}
    end.

%%% Closers (`;`)

op_close_test(#continuation{data_stack = [{'TestBody', Body} | Rest]} = Cont) ->
    Spec = build_spec(positive, Body, Rest),
    Cont#continuation{
        data_stack = Rest,
        test_registry = Cont#continuation.test_registry ++ [Spec]
    }.

op_close_test_raises(#continuation{data_stack = [{'TestRaisesBody', Body} | Rest]} = Cont) ->
    ErrType = maps:get(err_type, Body),
    Spec = build_spec({raises, ErrType}, Body, Rest),
    Cont#continuation{
        data_stack = Rest,
        test_registry = Cont#continuation.test_registry ++ [Spec]
    }.

op_close_setup(#continuation{data_stack = [{'SetupBody', Body},
                                           {'GroupScope', Frame} | Rest]} = Cont) ->
    Tokens = maps:get(body, Body),
    NewFrame = Frame#{setup => Tokens},
    Cont#continuation{
        data_stack = [{'GroupScope', NewFrame} | Rest]
    };
op_close_setup(_Cont) ->
    erlang:error({setup, not_in_group}).

op_close_teardown(#continuation{data_stack = [{'TeardownBody', Body},
                                              {'GroupScope', Frame} | Rest]} = Cont) ->
    Tokens = maps:get(body, Body),
    NewFrame = Frame#{teardown => Tokens},
    Cont#continuation{
        data_stack = [{'GroupScope', NewFrame} | Rest]
    };
op_close_teardown(_Cont) ->
    erlang:error({teardown, not_in_group}).

%%% Handlers

handle_test_pending(TokenValue, _Cont) ->
    erlang:error({test_pending, unexpected_token, TokenValue}).

handle_test_raises_pending(TokenValue, Cont) ->
    [{'TestRaisesPending', State} | Rest] = Cont#continuation.data_stack,
    case maps:get(err_type, State) of
        undefined ->
            NewState = State#{err_type => list_to_atom(TokenValue)},
            Cont#continuation{data_stack = [{'TestRaisesPending', NewState} | Rest]};
        _ ->
            erlang:error({test_raises_pending, unexpected_extra_token, TokenValue})
    end.

handle_body_capture(_TokenValue, Cont) ->
    [{BodyType, State} | Rest] = Cont#continuation.data_stack,
    Token = Cont#continuation.current_token,
    Body = maps:get(body, State),
    NewState = State#{body => Body ++ [Token]},
    Cont#continuation{data_stack = [{BodyType, NewState} | Rest]}.

%%% Helpers

build_spec(Kind, Body, StackAfterPop) ->
    {GroupPath, {Setup, Teardown}, Serial, Limits} = extract_group_info(StackAfterPop),
    #{
        group_path       => GroupPath,
        name             => maps:get(name, Body),
        kind             => Kind,
        body             => maps:get(body, Body),
        setup            => Setup,
        teardown         => Teardown,
        serial           => Serial,
        skip             => maps:get(skip, Body, undefined),
        max_depth        => maps:get(max_depth, Limits, undefined),
        max_return_depth => maps:get(max_return_depth, Limits, undefined),
        location         => maps:get(location, Body)
    }.

%% Collect GroupScope sentinels from the data stack, innermost-first as
%% they appear on the stack. The spec wants group_path outermost-first,
%% so reverse at the end. Setup / teardown come from the innermost group
%% (topmost on the stack). Serial flag is true if ANY enclosing group was
%% declared `group-serial`.
extract_group_info(Stack) ->
    Frames = [F || {'GroupScope', F} <- Stack, is_map(F)],
    Path = [maps:get(name, F) || F <- lists:reverse(Frames)],
    Serial = lists:any(fun(F) -> maps:get(serial, F, false) end, Frames),
    Limits = innermost_limits(Frames),
    case Frames of
        [Inner | _] ->
            {Path,
             {maps:get(setup, Inner, []), maps:get(teardown, Inner, [])},
             Serial,
             Limits};
        [] ->
            {[], {[], []}, false, #{}}
    end.

%% Depth limits from the nearest enclosing frame that sets them.
innermost_limits([]) -> #{};
innermost_limits([F | Rest]) ->
    L = innermost_limits(Rest),
    case {maps:get(max_depth, F, undefined),
          maps:get(max_return_depth, F, undefined)} of
        {undefined, undefined} -> L;
        {D, undefined}         -> L#{max_depth => D};
        {undefined, R}         -> L#{max_return_depth => R};
        {D, R}                 -> L#{max_depth => D, max_return_depth => R}
    end.

token_location(undefined) -> {undefined, 0};
token_location(#token{file = F, line = L}) -> {F, L}.

-module(af_type_test_dsl).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Test DSL — scope management + body capture.
%%
%% Syntax (Phase 2 subset of the full plan):
%%
%%   "group-name" group
%%       "test-name" test :
%%           ... test body ...
%%       ;
%%       "other test" test-raises stack_underflow :
%%           ... body that should raise ...
%%       ;
%%       setup :
%%           ... setup body ...
%%       ;
%%       teardown :
%%           ... teardown body ...
%%       ;
%%   end-group
%%
%% Deviations from plan:
%%   - Name precedes `group` for consistency with `test` / `test-raises`.
%%   - `end-group` explicit (rather than `;`) because `;` on data stack is
%%     ambiguous when TOS isn't a TestBody sentinel.
%%
%% Storage:
%%   - test_scope field holds the stack of active group names.
%%   - test_registry field accumulates test specs:
%%       #{group_path => [...], name => ..., kind => positive | {raises, Atom},
%%         body => [#token{}...], setup => [...], teardown => [...],
%%         location => {File, Line}}
%%   - During body capture, a TestBody / TestRaisesBody / SetupBody /
%%     TeardownBody sentinel is pinned to TOS. Its handler captures every
%%     incoming token as a #token{} record. `;` is registered on each body
%%     type to finalize.
%%
%% ETS policy: DSL words mutate ETS to register the body types and their
%% handlers at init time — but that's one-time registration, not per-test.
%% Per-test state (scope stack, registry, body captures) lives on the
%% continuation. Parallel test actors see separate continuations, so they
%% do not race on this state.

init() ->
    %% Register the intermediate state types FIRST so subsequent add_op
    %% calls find them in the registry.
    af_type:register_type(#af_type{name = 'TestPending'}),
    af_type:register_type(#af_type{name = 'TestRaisesPending'}),
    af_type:register_type(#af_type{name = 'TestBody'}),
    af_type:register_type(#af_type{name = 'TestRaisesBody'}),
    af_type:register_type(#af_type{name = 'SetupBody'}),
    af_type:register_type(#af_type{name = 'TeardownBody'}),

    %% Entry words on Any
    af_type:add_op('Any', #operation{
        name = "group", sig_in = ['String'], sig_out = [],
        impl = fun op_group/1, source = af_type_test_dsl
    }),
    af_type:add_op('Any', #operation{
        name = "end-group", sig_in = [], sig_out = [],
        impl = fun op_end_group/1, source = af_type_test_dsl
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

    %% Transition ops (`:`)
    af_type:add_op('TestPending', #operation{
        name = ":", sig_in = ['TestPending'], sig_out = ['TestBody'],
        impl = fun op_test_colon/1, source = af_type_test_dsl
    }),
    af_type:add_op('TestRaisesPending', #operation{
        name = ":", sig_in = ['TestRaisesPending'], sig_out = ['TestRaisesBody'],
        impl = fun op_test_raises_colon/1, source = af_type_test_dsl
    }),

    %% Closers (`;`)
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

    %% Register body types with handlers (re-register TestPending/etc. as
    %% already-registered types get their handler attached).
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

op_group(#continuation{data_stack = [{'String', Name} | Rest],
                       test_scope = Scope} = Cont) ->
    %% Push a new group frame onto test_scope. setup/teardown slots start
    %% empty; they get filled in by op_close_setup/op_close_teardown.
    Frame = #{name => Name, setup => [], teardown => []},
    Cont#continuation{
        data_stack = Rest,
        test_scope = [Frame | Scope]
    }.

op_end_group(#continuation{test_scope = [_ | Rest]} = Cont) ->
    Cont#continuation{test_scope = Rest};
op_end_group(_Cont) ->
    erlang:error({end_group, not_in_group}).

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

%%% Transitions (`:`)

op_test_colon(#continuation{data_stack = [{'TestPending', State} | Rest],
                            current_token = Tok} = Cont) ->
    Body = #{name     => maps:get(name, State),
             body     => [],
             location => token_location(Tok)},
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
    Spec = build_spec(positive, Body, Cont),
    Cont#continuation{
        data_stack = Rest,
        test_registry = Cont#continuation.test_registry ++ [Spec]
    }.

op_close_test_raises(#continuation{data_stack = [{'TestRaisesBody', Body} | Rest]} = Cont) ->
    ErrType = maps:get(err_type, Body),
    Spec = build_spec({raises, ErrType}, Body, Cont),
    Cont#continuation{
        data_stack = Rest,
        test_registry = Cont#continuation.test_registry ++ [Spec]
    }.

op_close_setup(#continuation{data_stack = [{'SetupBody', Body} | Rest],
                             test_scope = [Frame | ScopeRest]} = Cont) ->
    Tokens = maps:get(body, Body),
    NewFrame = Frame#{setup => Tokens},
    Cont#continuation{
        data_stack = Rest,
        test_scope = [NewFrame | ScopeRest]
    };
op_close_setup(_Cont) ->
    erlang:error({setup, not_in_group}).

op_close_teardown(#continuation{data_stack = [{'TeardownBody', Body} | Rest],
                                test_scope = [Frame | ScopeRest]} = Cont) ->
    Tokens = maps:get(body, Body),
    NewFrame = Frame#{teardown => Tokens},
    Cont#continuation{
        data_stack = Rest,
        test_scope = [NewFrame | ScopeRest]
    };
op_close_teardown(_Cont) ->
    erlang:error({teardown, not_in_group}).

%%% Handlers

%% TestPending handler — should rarely fire because `:` comes immediately
%% after `test`. A stray token here is a DSL syntax error.
handle_test_pending(TokenValue, _Cont) ->
    erlang:error({test_pending, unexpected_token, TokenValue}).

%% TestRaisesPending handler — captures the next token as the err-type atom.
handle_test_raises_pending(TokenValue, Cont) ->
    [{'TestRaisesPending', State} | Rest] = Cont#continuation.data_stack,
    case maps:get(err_type, State) of
        undefined ->
            NewState = State#{err_type => list_to_atom(TokenValue)},
            Cont#continuation{data_stack = [{'TestRaisesPending', NewState} | Rest]};
        _ ->
            erlang:error({test_raises_pending, unexpected_extra_token, TokenValue})
    end.

%% Universal body-capture handler — appends the original token record to
%% the body list. Works for TestBody / TestRaisesBody / SetupBody / TeardownBody
%% because all four carry a #{body => [#token{}...]} map.
handle_body_capture(_TokenValue, Cont) ->
    [{BodyType, State} | Rest] = Cont#continuation.data_stack,
    Token = Cont#continuation.current_token,
    Body = maps:get(body, State),
    NewState = State#{body => Body ++ [Token]},
    Cont#continuation{data_stack = [{BodyType, NewState} | Rest]}.

%%% Helpers

build_spec(Kind, Body, Cont) ->
    Scope = Cont#continuation.test_scope,
    GroupPath = [maps:get(name, F) || F <- lists:reverse(Scope)],
    {Setup, Teardown} = case Scope of
        [Frame | _] ->
            {maps:get(setup, Frame, []), maps:get(teardown, Frame, [])};
        [] ->
            {[], []}
    end,
    #{
        group_path => GroupPath,
        name       => maps:get(name, Body),
        kind       => Kind,
        body       => maps:get(body, Body),
        setup      => Setup,
        teardown   => Teardown,
        location   => maps:get(location, Body)
    }.

token_location(undefined) -> {undefined, 0};
token_location(#token{file = F, line = L}) -> {F, L}.

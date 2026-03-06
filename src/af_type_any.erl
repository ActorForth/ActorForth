-module(af_type_any).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

init() ->
    %% dup : Any -> Any, Any
    af_type:add_op('Any', #operation{
        name = "dup", sig_in = ['Any'], sig_out = ['Any', 'Any'],
        impl = fun op_dup/1
    }),

    %% drop : Any ->
    af_type:add_op('Any', #operation{
        name = "drop", sig_in = ['Any'], sig_out = [],
        impl = fun op_drop/1
    }),

    %% swap : Any, Any -> Any, Any
    af_type:add_op('Any', #operation{
        name = "swap", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any'],
        impl = fun op_swap/1
    }),

    %% rot : Any, Any, Any -> Any, Any, Any  (brings 3rd to top)
    af_type:add_op('Any', #operation{
        name = "rot", sig_in = ['Any', 'Any', 'Any'], sig_out = ['Any', 'Any', 'Any'],
        impl = fun op_rot/1
    }),

    %% over : Any, Any -> Any, Any, Any  (copies 2nd to top)
    af_type:add_op('Any', #operation{
        name = "over", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any', 'Any'],
        impl = fun op_over/1
    }),

    %% 2dup : Any, Any -> Any, Any, Any, Any
    af_type:add_op('Any', #operation{
        name = "2dup", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any', 'Any', 'Any'],
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
    [{_Type, Value} | Rest] = Cont#continuation.data_stack,
    io:format("~p~n", [Value]),
    Cont#continuation{data_stack = Rest}.

op_stack(Cont) ->
    Stack = Cont#continuation.data_stack,
    case Stack of
        [] -> io:format("Stack empty~n");
        _ ->
            io:format("Stack(~p):~n", [length(Stack)]),
            lists:foldl(fun({Type, Val}, Idx) ->
                io:format("  ~p) ~p : ~p~n", [Idx, Val, Type]),
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

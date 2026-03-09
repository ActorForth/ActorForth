-module(af_interpreter_coverage_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

setup() ->
    af_type:reset(),
    af_type_any:init(),
    af_type_int:init(),
    af_type_bool:init(),
    af_type_string:init(),
    af_type_compiler:init().

make_token(Value) ->
    #token{value = Value, line = 1, column = 1, file = "test"}.

make_quoted_token(Value) ->
    #token{value = Value, line = 1, column = 1, file = "test", quoted = true}.

%%% ===== Legacy ETS path (dictionary = undefined) =====
%%% Lines 78-111: interpret_token_ets

ets_path_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"ETS path: basic atom push", fun() ->
            %% dictionary = undefined triggers ETS path
            Cont = #continuation{dictionary = undefined},
            Token = make_token("foobar"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Atom', "foobar"}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS path: integer literal", fun() ->
            Cont = #continuation{dictionary = undefined},
            Token = make_token("42"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 42}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS path: dup operation found in Any dict", fun() ->
            Cont = #continuation{
                dictionary = undefined,
                data_stack = [{'Int', 5}]
            },
            Token = make_token("dup"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 5}, {'Int', 5}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS path: + operation found in TOS type dict", fun() ->
            Cont = #continuation{
                dictionary = undefined,
                data_stack = [{'Int', 3}, {'Int', 7}]
            },
            Token = make_token("+"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 10}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS path: quoted string becomes String (lines 96-98)", fun() ->
            Cont = #continuation{dictionary = undefined},
            Token = make_quoted_token("hello"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'String', <<"hello">>}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS path: handler invoked for type with handler (lines 86-87)", fun() ->
            %% Register a type with a handler via ETS
            HandlerType = #af_type{
                name = 'HandlerTest',
                ops = #{},
                handler = fun(Value, Cont0) ->
                    Cont0#continuation{
                        data_stack = [{'Atom', "handled:" ++ Value} |
                                      Cont0#continuation.data_stack]
                    }
                end
            },
            af_type:register_type(HandlerType),
            Cont = #continuation{
                dictionary = undefined,
                data_stack = [{'HandlerTest', test_val}]
            },
            Token = make_token("anytoken"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Atom', "handled:anytoken"}, {'HandlerTest', test_val}],
                         Result#continuation.data_stack)
        end} end
    ]}.

%%% ===== Debug trace formatting (lines 140-163) =====
%%% Exercise all format_dispatch paths and format_stack via debug=true.
%%% We verify the operation succeeds and produces correct results;
%%% the debug output goes to the group leader as a side effect,
%%% which is sufficient to cover the format_* functions.

debug_trace_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"debug mode: TOS dispatch covers format_dispatch tos (lines 154-155)", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [{'Int', 5}, {'Int', 3}],
                debug = true
            },
            Token = make_token("+"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 8}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: Any dispatch covers format_dispatch any", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [{'Int', 5}],
                debug = true
            },
            Token = make_token("dup"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 5}, {'Int', 5}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: literal dispatch covers format_dispatch literal (lines 161-162)", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [],
                debug = true
            },
            Token = make_token("42"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 42}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: atom dispatch covers format_dispatch atom (line 163)", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [],
                debug = true
            },
            Token = make_token("unknown_xyz"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Atom', "unknown_xyz"}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: handler dispatch covers format_dispatch handler", fun() ->
            HandlerType = #af_type{
                name = 'DebugHandler',
                ops = #{},
                handler = fun(_Value, Cont0) -> Cont0 end
            },
            af_type:register_type(HandlerType),
            Dict = af_type:snapshot(),
            Cont = #continuation{
                dictionary = Dict,
                data_stack = [{'DebugHandler', test}],
                debug = true
            },
            Token = make_token("something"),
            Result = af_interpreter:interpret_token(Token, Cont),
            %% Handler returns Cont0 unchanged
            ?assertEqual([{'DebugHandler', test}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: quoted string literal", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [],
                debug = true
            },
            Token = make_quoted_token("hello"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'String', <<"hello">>}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: format_stack with multiple items", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [{'Int', 5}, {'Bool', true}],
                debug = true
            },
            Token = make_token("dup"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Int', 5}, {'Int', 5}, {'Bool', true}],
                         Result#continuation.data_stack)
        end} end,
        fun(_) -> {"debug mode: format_stack with empty stack", fun() ->
            Cont = #continuation{
                dictionary = af_type:snapshot(),
                data_stack = [],
                debug = true
            },
            Token = make_token("hello"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Atom', "hello"}], Result#continuation.data_stack)
        end} end
    ]}.

%%% ===== get_tos_handler ETS version (line 171) =====

ets_handler_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"ETS get_tos_handler returns handler when type has one (line 171)", fun() ->
            HandlerFun = fun(_Value, Cont0) ->
                Cont0#continuation{
                    data_stack = [{'Atom', "intercepted"} |
                                  Cont0#continuation.data_stack]
                }
            end,
            HandlerType = #af_type{
                name = 'EtsHandler',
                ops = #{},
                handler = HandlerFun
            },
            af_type:register_type(HandlerType),
            %% Use ETS path (dictionary = undefined) with TOS = EtsHandler
            Cont = #continuation{
                dictionary = undefined,
                data_stack = [{'EtsHandler', val}]
            },
            Token = make_token("interceptme"),
            Result = af_interpreter:interpret_token(Token, Cont),
            ?assertEqual([{'Atom', "intercepted"}, {'EtsHandler', val}],
                         Result#continuation.data_stack)
        end} end,
        fun(_) -> {"ETS get_tos_handler returns none for empty stack", fun() ->
            Cont = #continuation{
                dictionary = undefined,
                data_stack = []
            },
            Token = make_token("hello"),
            Result = af_interpreter:interpret_token(Token, Cont),
            %% Should push as atom since no handler
            ?assertEqual([{'Atom', "hello"}], Result#continuation.data_stack)
        end} end
    ]}.

%%% ===== Misc: interpret_tokens, new_continuation =====

misc_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"new_continuation has snapshot dictionary", fun() ->
            Cont = af_interpreter:new_continuation(),
            ?assertNotEqual(undefined, Cont#continuation.dictionary),
            ?assertEqual([], Cont#continuation.data_stack)
        end} end,
        fun(_) -> {"interpret_tokens with empty list returns cont unchanged", fun() ->
            Cont = af_interpreter:new_continuation(),
            Result = af_interpreter:interpret_tokens([], Cont),
            ?assertEqual(Cont, Result)
        end} end,
        fun(_) -> {"interpret_tokens processes multiple tokens", fun() ->
            Cont = af_interpreter:new_continuation(),
            Tokens = [make_token("5"), make_token("int"),
                      make_token("3"), make_token("int"),
                      make_token("+")],
            Result = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Int', 8}], Result#continuation.data_stack)
        end} end
    ]}.


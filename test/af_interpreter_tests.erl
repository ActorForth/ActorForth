-module(af_interpreter_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

setup() -> af_type:reset().

%% --- basic interpretation ---

fallback_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"unknown tokens become Atoms", fun() ->
            Tokens = af_parser:parse("hello world", "test"),
            Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            ?assertEqual([{'Atom', "world"}, {'Atom', "hello"}],
                         Result#continuation.data_stack)
        end} end,
        fun(_) -> {"empty token list returns unchanged continuation", fun() ->
            Cont = af_interpreter:new_continuation(),
            ?assertEqual(Cont, af_interpreter:interpret_tokens([], Cont))
        end} end,
        fun(_) -> {"current_token is set to last processed token", fun() ->
            Tokens = af_parser:parse("hello", "myfile"),
            Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            ?assertEqual("hello", (Result#continuation.current_token)#token.value)
        end} end
    ]}.

%% --- dispatch ---

dispatch_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"dispatches to operation when found", fun() ->
            %% Register a "nop" op that does nothing
            NopOp = #operation{name = "nop", sig_in = [], sig_out = [],
                              impl = fun(C) -> C end},
            af_type:add_op('Any', NopOp),
            Tokens = af_parser:parse("hello nop", "test"),
            Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            %% "hello" pushed as Atom, "nop" executed (identity), stack unchanged
            ?assertEqual([{'Atom', "hello"}], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"operation can modify stack", fun() ->
            %% Register a "clear" op that empties the stack
            ClearOp = #operation{name = "clear", sig_in = [], sig_out = [],
                                impl = fun(C) -> C#continuation{data_stack = []} end},
            af_type:add_op('Any', ClearOp),
            Tokens = af_parser:parse("hello world clear", "test"),
            Result = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
            ?assertEqual([], Result#continuation.data_stack)
        end} end,
        fun(_) -> {"dispatch respects TOS type", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            %% "show" in Int dict expects Int on top
            IntShow = #operation{name = "show", sig_in = ['Int'], sig_out = ['Int'],
                                impl = fun(C) ->
                                    [{'Int', V} | _] = C#continuation.data_stack,
                                    C#continuation{return_stack = [V]}
                                end},
            af_type:add_op('Int', IntShow),
            %% Manually set up a stack with an Int
            Cont = #continuation{data_stack = [{'Int', 42}]},
            Tokens = af_parser:parse("show", "test"),
            Result = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([42], Result#continuation.return_stack)
        end} end,
        fun(_) -> {"falls through to Atom when sig doesn't match", fun() ->
            af_type:register_type(#af_type{name = 'Int'}),
            IntOp = #operation{name = "+", sig_in = ['Int', 'Int'], sig_out = ['Int'],
                              impl = fun(C) -> C end},
            af_type:add_op('Int', IntOp),
            %% Only one Int on stack, + needs two — should fall through to Atom
            Cont = #continuation{data_stack = [{'Int', 42}]},
            Tokens = af_parser:parse("+", "test"),
            Result = af_interpreter:interpret_tokens(Tokens, Cont),
            ?assertEqual([{'Atom', "+"}, {'Int', 42}], Result#continuation.data_stack)
        end} end
    ]}.

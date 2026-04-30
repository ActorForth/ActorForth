-module(af_interpreter_a4_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% Ext-1 (#180): #af_type{handler = atom()} dispatches a token
%% through the named a4 word instead of an Erlang fun.

setup() ->
    af_repl:init_types(),
    af_type_compiler:clear_pending_checks(),
    ok.

eval_new(Src) ->
    Tokens = af_parser:parse(Src, "t"),
    af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()).

a4_handler_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"a4-word handler receives the token value as a String on TOS", fun() ->
            %% Define an a4 word that stringifies whatever it sees:
            %% expects {String, TokenValue} on TOS (from the handler
            %% bridge), stashes it into a list stashed under a
            %% CollectorDef state.
            eval_new(": collector-push String -> ; drop ."),
            %% Register the CollectorDef type with an a4 word as handler.
            af_type:register_type(#af_type{
                name = 'CollectorDef',
                ops = #{},
                handler = 'collector-push',
                fields = []
            }),
            %% Register a word that pushes a CollectorDef state onto the stack.
            af_type:add_op('Any', #operation{
                name = "collector",
                sig_in = [],
                sig_out = ['CollectorDef'],
                impl = fun(Cont) ->
                    Stack = Cont#continuation.data_stack,
                    Cont#continuation{
                        data_stack = [{'CollectorDef', #{}} | Stack]
                    }
                end
            }),
            %% Exercise: `collector foo bar baz` should dispatch each
            %% of foo/bar/baz through collector-push (the a4 word).
            %% Each call pushes a String onto stack then drops it (in
            %% our trivial handler body). Stack ends with the original
            %% CollectorDef state unchanged.
            C = eval_new("collector foo bar baz"),
            [Top | _] = C#continuation.data_stack,
            ?assertMatch({'CollectorDef', _}, Top)
        end} end
    ]}.

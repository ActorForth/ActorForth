-module(af_type_test_dsl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("continuation.hrl").

eval(Input, Cont) ->
    Tokens = af_parser:parse(Input, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

setup() ->
    af_type:reset(),
    af_interpreter:new_continuation().

%%% Basic test registration

basic_test_() ->
    [
        {"`test :` ... `;` registers a single positive test", fun() ->
            C = eval("\"my test\" test : 1 2 + ;", setup()),
            Registry = C#continuation.test_registry,
            ?assertEqual(1, length(Registry)),
            [Spec] = Registry,
            ?assertEqual(<<"my test">>, maps:get(name, Spec)),
            ?assertEqual(positive, maps:get(kind, Spec)),
            ?assertEqual([], maps:get(group_path, Spec)),
            Body = maps:get(body, Spec),
            ?assertEqual(3, length(Body))
        end},
        {"stack is clean after closing the test", fun() ->
            C = eval("\"t\" test : 1 2 + ;", setup()),
            ?assertEqual([], C#continuation.data_stack)
        end},
        {"multiple tests accumulate in registry order", fun() ->
            C = eval("\"a\" test : 1 ; \"b\" test : 2 ; \"c\" test : 3 ;", setup()),
            Names = [maps:get(name, S) || S <- C#continuation.test_registry],
            ?assertEqual([<<"a">>, <<"b">>, <<"c">>], Names)
        end}
    ].

%%% Group scope

group_test_() ->
    [
        {"tests inside a group carry the group name", fun() ->
            Src = "\"list_ops\" group "
                  "\"cons basic\" test : 1 ; "
                  "\"head basic\" test : 2 ; "
                  "end-group",
            C = eval(Src, setup()),
            Specs = C#continuation.test_registry,
            ?assertEqual(2, length(Specs)),
            Paths = [maps:get(group_path, S) || S <- Specs],
            ?assertEqual([[<<"list_ops">>], [<<"list_ops">>]], Paths)
        end},
        {"end-group pops the scope; tests after it lose the group name", fun() ->
            Src = "\"g1\" group \"inside\" test : ; end-group \"outside\" test : ;",
            C = eval(Src, setup()),
            [In, Out] = C#continuation.test_registry,
            ?assertEqual([<<"g1">>], maps:get(group_path, In)),
            ?assertEqual([], maps:get(group_path, Out))
        end},
        {"nested groups produce nested group paths", fun() ->
            Src = "\"outer\" group \"inner\" group \"deep test\" test : ; "
                  "end-group end-group",
            C = eval(Src, setup()),
            [Spec] = C#continuation.test_registry,
            ?assertEqual([<<"outer">>, <<"inner">>],
                         maps:get(group_path, Spec))
        end}
    ].

%%% test-raises

test_raises_test_() ->
    [
        {"test-raises captures err-type atom", fun() ->
            Src = "\"should fail\" test-raises stack_underflow : drop ;",
            C = eval(Src, setup()),
            [Spec] = C#continuation.test_registry,
            ?assertEqual({raises, stack_underflow}, maps:get(kind, Spec))
        end}
    ].

%%% setup / teardown

setup_teardown_test_() ->
    [
        {"setup body attached to tests in group", fun() ->
            Src = "\"g\" group "
                  "setup : 99 ; "
                  "\"t\" test : 1 ; "
                  "end-group",
            C = eval(Src, setup()),
            [Spec] = C#continuation.test_registry,
            Setup = maps:get(setup, Spec),
            ?assert(length(Setup) > 0)
        end},
        {"teardown body attached to tests in group", fun() ->
            Src = "\"g\" group "
                  "teardown : drop ; "
                  "\"t\" test : 1 ; "
                  "end-group",
            C = eval(Src, setup()),
            [Spec] = C#continuation.test_registry,
            Teardown = maps:get(teardown, Spec),
            ?assert(length(Teardown) > 0)
        end}
    ].

%%% Body capture fidelity

body_capture_test_() ->
    [
        {"captured body preserves quoted-string tokens", fun() ->
            C = eval("\"t\" test : \"hello\" print ;", setup()),
            [Spec] = C#continuation.test_registry,
            Body = maps:get(body, Spec),
            %% Expect 2 body tokens: "hello" (quoted) and print
            ?assertEqual(2, length(Body))
        end},
        {"captured body preserves location info", fun() ->
            C = eval("\"t\" test : 1 ;", setup()),
            [Spec] = C#continuation.test_registry,
            {_File, Line} = maps:get(location, Spec),
            ?assertEqual(1, Line)
        end}
    ].

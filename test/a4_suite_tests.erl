-module(a4_suite_tests).

-include_lib("eunit/include/eunit.hrl").

%% Runs all .test.a4 under lib/testing via the a4 runner and fails
%% this EUnit test if any a4 test fails. This lets `rebar3 eunit`
%% cover both the Erlang and a4 suites in a single invocation.

a4_suite_test_() ->
    {timeout, 120, fun() ->
        {Passed, Failed, Errors} = af_test_runner:run(
            ["lib/testing"],
            [{render, quiet}]),
        ?assert(Passed > 0),
        ?assertEqual(0, Failed),
        ?assertEqual(0, Errors)
    end}.

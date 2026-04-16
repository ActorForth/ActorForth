-module(af_cli_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test arg parsing

parse_args_test_() ->
    [
        {"empty args", fun() ->
            {Files, Opts} = af_cli:parse_args([]),
            ?assertEqual([], Files),
            ?assertEqual([], Opts)
        end},
        {"single file", fun() ->
            {Files, _Opts} = af_cli:parse_args(["test.a4"]),
            ?assertEqual(["test.a4"], Files)
        end},
        {"file with -o", fun() ->
            {Files, Opts} = af_cli:parse_args(["test.a4", "-o", "/tmp"]),
            ?assertEqual(["test.a4"], Files),
            ?assertEqual("/tmp", proplists:get_value(o, Opts))
        end},
        {"selfhosted flag", fun() ->
            {_Files, Opts} = af_cli:parse_args(["test.a4", "--selfhosted"]),
            ?assert(proplists:get_bool(selfhosted, Opts))
        end},
        {"multiple files", fun() ->
            {Files, _Opts} = af_cli:parse_args(["a.a4", "b.a4"]),
            ?assertEqual(["a.a4", "b.a4"], Files)
        end}
    ].

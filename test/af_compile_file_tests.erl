-module(af_compile_file_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    af_type:reset(),
    af_repl:init_types().

compile_file_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compile .a4 file to module", fun() ->
            File = "test/test_lib.a4",
            {ok, test_lib} = af_compile_file:compile(File),
            ?assertEqual(10, test_lib:double(5)),
            ?assertEqual(49, test_lib:square(7)),
            ?assertEqual(6, test_lib:inc(5))
        end} end,

        fun(_) -> {"compile .a4 file with custom module name", fun() ->
            File = "test/test_lib.a4",
            {ok, af_test_math} = af_compile_file:compile(File, af_test_math),
            ?assertEqual(10, af_test_math:double(5)),
            ?assertEqual(49, af_test_math:square(7))
        end} end,

        fun(_) -> {"compile .a4 file to directory", fun() ->
            File = "test/test_lib.a4",
            TmpDir = "/tmp/af_compile_test_" ++ integer_to_list(erlang:unique_integer([positive])),
            {ok, test_lib} = af_compile_file:compile_to_dir(File, TmpDir),
            BeamFile = filename:join(TmpDir, "test_lib.beam"),
            ?assert(filelib:is_regular(BeamFile)),
            %% Clean up
            file:delete(BeamFile),
            file:del_dir(TmpDir)
        end} end,

        fun(_) -> {"compile nonexistent file returns error", fun() ->
            {error, {read_error, _, enoent}} = af_compile_file:compile("nonexistent.a4")
        end} end
    ]}.

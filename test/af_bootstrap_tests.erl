-module(af_bootstrap_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test the full bootstrap: parser.a4 + compiler.a4 compile themselves
%% through the A4-written pipeline (zero dependency on af_parser/af_interpreter).

setup() ->
    af_type:reset().

%% Compile parser.a4 and compiler.a4 to BEAM via the Erlang self-hosted pipeline
compile_bootstrap_modules() ->
    {ok, P} = af_ring2:compile_file_selfhosted("src/bootstrap/parser.a4", "parser"),
    {ok, C} = af_ring2:compile_file_selfhosted("src/bootstrap/compiler.a4", "compiler"),
    {P, C}.

%% Helper: parse source using A4-compiled parser
parse_source(ParserMod, Source) ->
    Result = ParserMod:parse([{'String', Source}, {'String', <<"test">>}]),
    element(2, hd(Result)).

%% Helper: compile tokens using A4-compiled compiler
compile_tokens(CompilerMod, Tokens) ->
    Result = CompilerMod:'compile-tokens'([{'List', Tokens}]),
    element(2, hd(Result)).

%% === Basic pipeline tests ===

parser_compiles_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"parser.a4 compiles to BEAM", fun() ->
            {ok, P} = af_ring2:compile_file_selfhosted("src/bootstrap/parser.a4", "parser"),
            Exports = [F || {F, _} <- P:module_info(exports), F =/= module_info],
            ?assert(lists:member(parse, Exports)),
            ?assert(lists:member(tokenize, Exports))
        end}
    end}.

compiler_compiles_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"compiler.a4 compiles to BEAM", fun() ->
            {ok, C} = af_ring2:compile_file_selfhosted("src/bootstrap/compiler.a4", "compiler"),
            Exports = [F || {F, _} <- C:module_info(exports), F =/= module_info],
            ?assert(lists:member('compile-tokens', Exports)),
            ?assert(lists:member('compile-loop', Exports))
        end}
    end}.

%% === Parser correctness ===

parser_tokenizes_simple_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"parser tokenizes simple expression", fun() ->
            {P, _C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<"1 2 +">>),
            ?assertEqual(3, length(Tokens)),
            %% Check first token value
            T1 = element(2, hd(Tokens)),
            V1 = maps:get({'String', <<"value">>}, T1),
            ?assertEqual({'String', <<"1">>}, V1)
        end}
    end}.

parser_handles_strings_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"parser handles quoted strings", fun() ->
            {P, _C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<"\"hello\" print">>),
            ?assertEqual(2, length(Tokens)),
            T1 = element(2, hd(Tokens)),
            Q1 = maps:get({'String', <<"quoted">>}, T1),
            ?assertEqual({'Bool', true}, Q1)
        end}
    end}.

parser_handles_comments_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"parser skips comments", fun() ->
            {P, _C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<"# comment\n1 2">>),
            ?assertEqual(2, length(Tokens))
        end}
    end}.

%% === Compiler correctness ===

compiler_simple_word_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"compiler compiles a simple word", fun() ->
            {P, C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<": double Int -> Int ; dup + .">>),
            Defs = compile_tokens(C, Tokens),
            ?assertEqual(1, length(Defs)),
            Def = element(2, hd(Defs)),
            Name = maps:get({'String', <<"name">>}, Def),
            ?assertEqual({'String', <<"double">>}, Name)
        end}
    end}.

compiler_product_type_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"compiler compiles a product type", fun() ->
            {P, C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<"type Point x Int y Int .">>),
            Defs = compile_tokens(C, Tokens),
            %% Constructor + 2 getters + 2 setters = 5
            ?assertEqual(5, length(Defs))
        end}
    end}.

compiler_subclauses_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"compiler compiles sub-clauses", fun() ->
            {P, C} = compile_bootstrap_modules(),
            Tokens = parse_source(P, <<": fact Int -> Int ;\n"
                                        "  : 0 -> Int ; drop 1\n"
                                        "  : Int -> Int ; dup 1 - fact * .">>),
            Defs = compile_tokens(C, Tokens),
            ?assertEqual(1, length(Defs)),
            Def = element(2, hd(Defs)),
            Body = maps:get({'String', <<"body">>}, Def),
            ?assertEqual({'List', _} = Body, Body),
            %% Body should contain a select_clause instruction
            BodyList = element(2, Body),
            ?assert(length(BodyList) > 0)
        end}
    end}.

%% === Self-compilation tests ===

parser_self_compiles_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"parser.a4 self-compiles through A4 pipeline", fun() ->
            {P, C} = compile_bootstrap_modules(),
            {ok, Src} = file:read_file("src/bootstrap/parser.a4"),
            Tokens = parse_source(P, Src),
            ?assert(length(Tokens) > 100),
            Defs = compile_tokens(C, Tokens),
            ?assert(length(Defs) >= 49)
        end}
    end}.

compiler_self_compiles_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, fun(_) ->
        {"compiler.a4 self-compiles through A4 pipeline", fun() ->
            {P, C} = compile_bootstrap_modules(),
            {ok, Src} = file:read_file("src/bootstrap/compiler.a4"),
            Tokens = parse_source(P, Src),
            ?assert(length(Tokens) > 100),
            Defs = compile_tokens(C, Tokens),
            ?assert(length(Defs) >= 99)
        end}
    end}.

%% === Full bootstrap: compile to BEAM and verify output matches ===

full_bootstrap_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     {timeout, 60, fun() ->
        %% Stage 1: Compile parser.a4 + compiler.a4 via Erlang pipeline
        {P1, C1} = compile_bootstrap_modules(),

        %% Stage 2: Use A4 parser+compiler to compile parser.a4
        {ok, PSrc} = file:read_file("src/bootstrap/parser.a4"),
        PToks1 = parse_source(P1, PSrc),
        PDefs1 = compile_tokens(C1, PToks1),

        %% Stage 2: Use A4 parser+compiler to compile compiler.a4
        {ok, CSrc} = file:read_file("src/bootstrap/compiler.a4"),
        CToks1 = parse_source(P1, CSrc),
        CDefs1 = compile_tokens(C1, CToks1),

        %% Verify definition counts are reasonable
        ?assert(length(PDefs1) >= 49),
        ?assert(length(CDefs1) >= 99),

        %% Verify definition names include key entry points
        PNames = [maps:get({'String', <<"name">>}, element(2, D)) || D <- PDefs1],
        ?assert(lists:member({'String', <<"parse">>}, PNames)),
        ?assert(lists:member({'String', <<"tokenize">>}, PNames)),

        CNames = [maps:get({'String', <<"name">>}, element(2, D)) || D <- CDefs1],
        ?assert(lists:member({'String', <<"compile-tokens">>}, CNames)),
        ?assert(lists:member({'String', <<"compile-loop">>}, CNames)),
        ?assert(lists:member({'String', <<"compile-product-type">>}, CNames))
    end}}.

%% === Self-compilation: compiler.a4 compiles itself to BEAM ===

self_compilation_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     {timeout, 120, fun() ->
        %% Stage 1: Compile parser.a4 + compiler.a4 via Erlang selfhosted pipeline
        compile_bootstrap_modules(),

        %% Stage 2: compiler.a4 compiles itself via A4 compiler bridge
        {ok, SelfMod} = af_ring2:compile_file_via_a4_compiler(
            "src/bootstrap/compiler.a4", "self_compiler"),
        ?assert(is_atom(SelfMod)),

        %% Stage 3: Self-compiled compiler produces correct output
        {ok, CSrc} = file:read_file("src/bootstrap/compiler.a4"),
        [{_, Tokens}] = af_r2_parser:parse(
            [{'String', CSrc}, {'String', <<"compiler.a4">>}]),
        [{'List', SelfDefs}] = SelfMod:'compile-tokens'([{'List', Tokens}]),
        ?assertEqual(99, length(SelfDefs)),

        %% Stage 4: Compare with original compiler's output (names must match)
        [{_, Tokens2}] = af_r2_parser:parse(
            [{'String', CSrc}, {'String', <<"compiler.a4">>}]),
        [{'List', OrigDefs}] = af_r2_compiler:'compile-tokens'([{'List', Tokens2}]),
        OrigNames = [maps:get({'String', <<"name">>}, M) || {'Map', M} <- OrigDefs],
        SelfNames = [maps:get({'String', <<"name">>}, M) || {'Map', M} <- SelfDefs],
        ?assertEqual(OrigNames, SelfNames),

        %% Stage 5: Self-compiled compiler can compile programs with product types
        TestSrc = <<"type Pair a Int b String . : mk Int String -> Pair ; pair .">>,
        [{_, TToks}] = af_r2_parser:parse(
            [{'String', TestSrc}, {'String', <<"test">>}]),
        [{'List', TDefs}] = SelfMod:'compile-tokens'([{'List', TToks}]),
        %% Constructor + 2 getters + 2 setters + mk = 6
        ?assertEqual(6, length(TDefs)),
        TNames = [maps:get({'String', <<"name">>}, M) || {'Map', M} <- TDefs],
        ?assert(lists:member({'String', <<"pair">>}, TNames)),
        ?assert(lists:member({'String', <<"a">>}, TNames)),
        ?assert(lists:member({'String', <<"b">>}, TNames)),
        ?assert(lists:member({'String', <<"a!">>}, TNames)),
        ?assert(lists:member({'String', <<"b!">>}, TNames)),
        ?assert(lists:member({'String', <<"mk">>}, TNames))
    end}}.

%% === Driver tests: A4-written compilation driver ===

driver_compiles_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     {timeout, 30, fun() ->
        {"driver.a4 compiles to BEAM", fun() ->
            compile_bootstrap_modules(),
            {ok, D} = af_ring2:compile_file_selfhosted("src/bootstrap/driver.a4", "driver"),
            Exports = [F || {F, _} <- D:module_info(exports), F =/= module_info],
            ?assert(lists:member('compile-file', Exports)),
            ?assert(lists:member('compile-source', Exports))
        end}
    end}}.

driver_compiles_simple_file_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     {timeout, 60, fun() ->
        {"driver.a4 compiles a simple .a4 file end-to-end", fun() ->
            compile_bootstrap_modules(),
            {ok, _} = af_ring2:compile_file_selfhosted("src/bootstrap/driver.a4", "driver"),

            %% Write a test file
            Source = <<": double Int -> Int ; dup + .">>,
            file:write_file("/tmp/test_driver_bootstrap.a4", Source),

            %% Use A4 driver to compile it
            Result = af_r2_driver:'compile-file'(
                [{'String', <<"test_drvr">>},
                 {'String', <<"/tmp/test_driver_bootstrap.a4">>}]),
            ?assertMatch([{'Bool', true} | _], Result),

            %% Verify compiled module works
            TestResult = af_r2_test_drvr:double([{'Int', 21}]),
            ?assertMatch([{'Int', 42} | _], TestResult)
        end}
    end}}.

%% === Full self-hosted bootstrap: all 4 modules compile themselves ===

selfhosted_full_bootstrap_test_() ->
    {setup, fun setup/0, fun(_) -> ok end,
     {timeout, 120, fun() ->
        %% Stage 1: Compile all 4 bootstrap modules via Erlang pipeline
        {ok, _} = af_ring2:compile_file_selfhosted("src/bootstrap/parser.a4", "parser"),
        {ok, _} = af_ring2:compile_file_selfhosted("src/bootstrap/compiler.a4", "compiler"),
        {ok, _} = af_ring2:compile_file_selfhosted("src/bootstrap/codegen.a4", "codegen"),
        {ok, _} = af_ring2:compile_file_selfhosted("src/bootstrap/driver.a4", "driver"),

        %% Stage 2: Use the A4 driver to self-compile all 4 bootstrap modules
        %% This uses the A4-written parser, compiler, codegen, and driver
        %% to compile themselves — the ultimate self-hosting test
        S2Parser = af_r2_driver:'compile-file'(
            [{'String', <<"s2_parser">>},
             {'String', <<"src/bootstrap/parser.a4">>}]),
        ?assertMatch([{'Bool', true} | _], S2Parser),

        S2Compiler = af_r2_driver:'compile-file'(
            [{'String', <<"s2_compiler">>},
             {'String', <<"src/bootstrap/compiler.a4">>}]),
        ?assertMatch([{'Bool', true} | _], S2Compiler),

        S2Codegen = af_r2_driver:'compile-file'(
            [{'String', <<"s2_codegen">>},
             {'String', <<"src/bootstrap/codegen.a4">>}]),
        ?assertMatch([{'Bool', true} | _], S2Codegen),

        S2Driver = af_r2_driver:'compile-file'(
            [{'String', <<"s2_driver">>},
             {'String', <<"src/bootstrap/driver.a4">>}]),
        ?assertMatch([{'Bool', true} | _], S2Driver),

        %% Stage 3: Use the self-compiled toolchain to compile fresh A4 code
        Source = <<": square Int -> Int ; dup * .\n"
                   ": cube Int -> Int ; dup square * .">>,
        file:write_file("/tmp/test_selfhosted_bootstrap.a4", Source),

        S3Result = af_r2_s2_driver:'compile-file'(
            [{'String', <<"s3_test">>},
             {'String', <<"/tmp/test_selfhosted_bootstrap.a4">>}]),
        ?assertMatch([{'Bool', true} | _], S3Result),

        %% Verify the compiled module works correctly
        ?assertMatch([{'Int', 49} | _], af_r2_s3_test:square([{'Int', 7}])),
        ?assertMatch([{'Int', 27} | _], af_r2_s3_test:cube([{'Int', 3}]))
    end}}.

-module(af_cpp_compiler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

setup() ->
    af_type:reset(),
    af_repl:init_types().

eval(Code, Cont) ->
    Tokens = af_parser:parse(Code, "test"),
    af_interpreter:interpret_tokens(Tokens, Cont).

%% Test basic word compilation to C++
basic_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compiles simple word to C++", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": double Int -> Int ; dup + .", C0),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            ?assert(length(WordDefs) >= 1),
            CppCode = af_cpp_compiler:generate_cpp("test_basic", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            %% Should contain function declaration
            ?assert(string:find(Code, "// Word: double") =/= nomatch),
            %% Should contain dup and + operations
            ?assert(string:find(Code, "push(s, peek(s))") =/= nomatch),
            ?assert(string:find(Code, "as_int() + a.as_int()") =/= nomatch),
            _ = C1
        end} end
    ]}.

%% Test arithmetic operations
arithmetic_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates arithmetic ops", fun() ->
            C0 = af_interpreter:new_continuation(),
            _C1 = eval(": calc Int Int -> Int ; + .", C0),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            CppCode = af_cpp_compiler:generate_cpp("test_arith", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            ?assert(string:find(Code, "as_int() + a.as_int()") =/= nomatch)
        end} end,
        fun(_) -> {"generates comparison ops", fun() ->
            C0 = af_interpreter:new_continuation(),
            _C1 = eval(": gt? Int Int -> Bool ; > .", C0),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            CppCode = af_cpp_compiler:generate_cpp("test_cmp", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            ?assert(string:find(Code, "as_int() > a.as_int()") =/= nomatch)
        end} end
    ]}.

%% Test product type generation
product_type_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates product type struct", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Point x Int y Int .", C0),
            ProductTypes = af_cpp_compiler:find_all_product_types(),
            ?assert(length(ProductTypes) >= 1),
            CppCode = af_cpp_compiler:generate_cpp("test_product", #{words => [], types => ProductTypes}),
            Code = binary_to_list(CppCode),
            %% Should have constructor
            ?assert(string:find(Code, "make_point") =/= nomatch),
            %% Should have getter functions
            ?assert(string:find(Code, "op_point_x") =/= nomatch),
            ?assert(string:find(Code, "op_point_y") =/= nomatch),
            %% Should have setter functions
            ?assert(string:find(Code, "op_point_x_set") =/= nomatch),
            _ = C1
        end} end
    ]}.

%% Test multi-clause (pattern matching) word
multi_clause_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates multi-clause function", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": fact 0 Int -> Int ; drop 1 .", C0),
            _C2 = eval(": fact Int -> Int ; dup 1 - fact * .", C1),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            FactDefs = [D || {N, _, _, _} = D <- WordDefs, N =:= "fact"],
            ?assert(length(FactDefs) >= 2),
            CppCode = af_cpp_compiler:generate_cpp("test_multi", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            %% Should have multi-clause comment
            ?assert(string:find(Code, "multi-clause") =/= nomatch),
            %% Should have value constraint check
            ?assert(string:find(Code, "make_int(0)") =/= nomatch)
        end} end
    ]}.

%% Test string operations
string_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates string concat", fun() ->
            C0 = af_interpreter:new_continuation(),
            _C1 = eval(": greet String -> String ; \"!\" concat .", C0),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            CppCode = af_cpp_compiler:generate_cpp("test_string", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            ?assert(string:find(Code, "as_string() + a.as_string()") =/= nomatch)
        end} end
    ]}.

%% Test file compilation
file_compile_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compiles .a4 file to .cpp", fun() ->
            %% Write a simple test file
            TestA4 = "/tmp/test_cpp_compile.a4",
            TestCpp = "/tmp/test_cpp_compile.cpp",
            ok = file:write_file(TestA4,
                ": square Int -> Int ; dup * .\n"
                ": cube Int -> Int ; dup dup * * .\n"),
            {ok, _} = af_cpp_compiler:compile_file_to_cpp(TestA4, TestCpp),
            {ok, Content} = file:read_file(TestCpp),
            Code = binary_to_list(Content),
            ?assert(string:find(Code, "square") =/= nomatch),
            ?assert(string:find(Code, "cube") =/= nomatch),
            ?assert(string:find(Code, "#include") =/= nomatch),
            %% Cleanup
            file:delete(TestA4),
            file:delete(TestCpp)
        end} end
    ]}.

%% Test the header/value type generation
header_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates complete header with value type", fun() ->
            CppCode = af_cpp_compiler:generate_cpp("test_header", #{words => [], types => []}),
            Code = binary_to_list(CppCode),
            ?assert(string:find(Code, "namespace af") =/= nomatch),
            ?assert(string:find(Code, "struct Value") =/= nomatch),
            ?assert(string:find(Code, "make_int") =/= nomatch),
            ?assert(string:find(Code, "make_string") =/= nomatch),
            ?assert(string:find(Code, "as_int()") =/= nomatch),
            ?assert(string:find(Code, "Stack") =/= nomatch),
            ?assert(string:find(Code, "#include <variant>") =/= nomatch)
        end} end
    ]}.

%% Test word with product type operations
product_word_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"compiles word using product type getters", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Counter count Int .", C0),
            _C2 = eval(": bump Counter -> Counter ; count 1 + count! .", C1),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            ProductTypes = af_cpp_compiler:find_all_product_types(),
            CppCode = af_cpp_compiler:generate_cpp("test_product_word",
                #{words => WordDefs, types => ProductTypes}),
            Code = binary_to_list(CppCode),
            %% Should reference the getter and setter
            ?assert(string:find(Code, "op_counter_count") =/= nomatch),
            _ = C1
        end} end
    ]}.

%% Test stack operations
stack_ops_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates all stack operations", fun() ->
            C0 = af_interpreter:new_continuation(),
            _C1 = eval(": test-ops Int Int Int -> Int ; rot drop swap over 2dup drop drop drop drop .", C0),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            CppCode = af_cpp_compiler:generate_cpp("test_stack", #{words => WordDefs, types => []}),
            Code = binary_to_list(CppCode),
            %% Check various stack ops are present
            ?assert(string:find(Code, "peek(s)") =/= nomatch orelse
                    string:find(Code, "pop(s)") =/= nomatch)
        end} end
    ]}.

%% Test cpp_function_name conversion
function_name_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"converts special chars in function names", fun() ->
            %% Test name with hyphens
            CppCode = af_cpp_compiler:generate_cpp("test_names",
                #{words => [{"map-get", ['Map'], ['Any'], []}], types => []}),
            Code = binary_to_list(CppCode),
            ?assert(string:find(Code, "map_get") =/= nomatch)
        end} end
    ]}.

%% Test complete compilation of the factorial example
factorial_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generates compilable factorial", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval(": factorial 0 Int -> Int ; drop 1 .", C0),
            _C2 = eval(": factorial Int -> Int ; dup 1 - factorial * .", C1),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            CppCode = af_cpp_compiler:generate_cpp("factorial", #{words => WordDefs, types => []}),
            _Code = binary_to_list(CppCode),
            %% Write to file and try to compile with g++
            TestCpp = "/tmp/test_factorial.cpp",
            ok = file:write_file(TestCpp, CppCode),
            CompileResult = os:cmd("g++ -std=c++20 -fsyntax-only -DAF_NO_MAIN " ++ TestCpp ++ " 2>&1"),
            ?assertEqual("", CompileResult),
            file:delete(TestCpp),
            _ = C1
        end} end
    ]}.

%% Test that generated code compiles with g++
compilable_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end, [
        fun(_) -> {"generated code compiles with g++", fun() ->
            C0 = af_interpreter:new_continuation(),
            C1 = eval("type Point x Int y Int .", C0),
            C2 = eval(": double Int -> Int ; dup + .", C1),
            _C3 = eval(": add-points Point Point -> Point ; swap x rot x + swap y rot y + point .", C2),
            WordDefs = af_cpp_compiler:find_all_compiled_words(),
            ProductTypes = af_cpp_compiler:find_all_product_types(),
            CppCode = af_cpp_compiler:generate_cpp("full_test",
                #{words => WordDefs, types => ProductTypes}),
            TestCpp = "/tmp/test_full_a4.cpp",
            ok = file:write_file(TestCpp, CppCode),
            CompileResult = os:cmd("g++ -std=c++20 -fsyntax-only -DAF_NO_MAIN " ++ TestCpp ++ " 2>&1"),
            case CompileResult of
                "" -> ok;
                Errors ->
                    io:format("C++ compilation errors:~n~s~n", [Errors]),
                    %% Don't fail — g++ may not be available
                    case os:find_executable("g++") of
                        false -> ok;  %% No g++ available, skip
                        _ -> ?assertEqual("", Errors)
                    end
            end,
            file:delete(TestCpp)
        end} end
    ]}.

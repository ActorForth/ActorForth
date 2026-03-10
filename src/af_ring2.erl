%% af_ring2.erl -- Ring 2: A4 Compiler targeting Ring 0/Ring 1
%%
%% Two compilation modes:
%%   parasitic (default) - delegates to Erlang parser/interpreter, then
%%                         translates compiled word bodies to Ring 0
%%   selfhosted          - uses af_r0_parser + af_r0_compiler directly,
%%                         no dependency on af_parser/af_interpreter
%%
%% Usage:
%%   {ok, Mod} = af_ring2:compile(": double Int -> Int ; dup + .", "test", "my_mod").
%%   {ok, Mod} = af_ring2:compile_selfhosted(": double Int -> Int ; dup + .", "test", "my_mod").
%%   Mod:double([{'Int', 21}]).  %% => [{'Int', 42}]

-module(af_ring2).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([
    compile/3,
    compile_selfhosted/3,
    compile_via_a4_compiler/3,
    compile_file/2,
    compile_file/3,
    compile_file_selfhosted/2,
    compile_file_selfhosted/3,
    compile_file_via_a4_compiler/2,
    compile_from_a4_defs/1,
    translate_word/2,
    translate_body/2,
    retranslate_bodies/2,
    merge_multi_clause/1
]).

%%% === Public API ===

%% Compile A4 source to a BEAM module via Ring 0/Ring 1.
compile(Source, Filename, ModName) ->
    ModAtom = list_to_atom("af_r2_" ++ to_str(ModName)),
    try
        %% Phase 1: Parse and interpret to define words
        Tokens = af_parser:parse(to_str(Source), to_str(Filename)),
        Cont = af_interpreter:new_continuation(),
        _FinalCont = af_interpreter:interpret_tokens(Tokens, Cont),
        %% Phase 2: Extract compiled word definitions
        WordDefs = find_all_compiled_words(),
        case WordDefs of
            [] -> {error, no_words_defined};
            _ ->
                %% Phase 3: Translate to Ring 0 instruction sequences
                WordNames = lists:usort([N || {N, _, _, _} <- WordDefs]),
                R0Defs = [translate_word(WD, WordNames) || WD <- WordDefs],
                %% Phase 3.5: Merge multi-clause words into select_clause
                MergedDefs = merge_multi_clause(R0Defs),
                %% Phase 4: Compile via Ring 1 (native mode)
                af_ring1:compile_module(ModAtom, MergedDefs, [{mode, native}])
        end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Compile an A4 file to a BEAM module.
compile_file(FilePath, ModName) ->
    compile_file(FilePath, ModName, []).

compile_file(FilePath, ModName, _Opts) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            compile(binary_to_list(Content), FilePath, ModName);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%%% === Self-Hosted Compilation ===
%%% Uses af_r0_parser + af_r0_compiler instead of af_parser + af_interpreter.
%%% No dependency on the Erlang interpreter infrastructure.

compile_selfhosted(Source, Filename, ModName) ->
    ModAtom = list_to_atom("af_r2_" ++ to_str(ModName)),
    try
        SourceBin = to_bin(Source),
        FileBin = to_bin(Filename),
        %% Phase 1: Parse with self-hosted parser
        Tokens = af_r0_parser:parse(SourceBin, FileBin),
        %% Phase 2: Compile tokens to Ring 0 word definitions
        WordDefs0 = af_r0_compiler:compile_tokens(Tokens),
        case WordDefs0 of
            [] -> {error, no_words_defined};
            _ ->
                %% Phase 2.5: Enrich word names for inter-word calls
                AllNames = lists:usort([N || {N, _, _, _} <- WordDefs0]),
                WordDefs = retranslate_bodies(WordDefs0, AllNames),
                %% Phase 3: Merge multi-clause words
                MergedDefs = merge_multi_clause(WordDefs),
                %% Phase 4: Compile via Ring 1 (native mode — no interpreter dependency)
                af_ring1:compile_module(ModAtom, MergedDefs, [{mode, native}])
        end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%%% === A4-Compiler Bridge ===
%%% Converts map-based output from compiler.a4 to tuple format for Ring 2.
%%% compiler.a4 produces: [{'Map', #{...}}] where maps have String-tagged keys
%%% Ring 2 expects: [{Name, SigIn, SigOut, Body}] with atom-based instructions

compile_via_a4_compiler(Source, Filename, ModName) ->
    ModAtom = list_to_atom("af_r2_" ++ to_str(ModName)),
    try
        SourceBin = to_bin(Source),
        FileBin = to_bin(Filename),
        %% Phase 1: Parse with parser.a4
        [{'List', Tokens}] = af_r2_parser:parse([{'String', SourceBin}, {'String', FileBin}]),
        %% Phase 2: Compile with compiler.a4
        [{'List', DefMaps}] = af_r2_compiler:'compile-tokens'([{'List', Tokens}]),
        %% Phase 3: Convert map format to tuple format
        WordDefs0 = [convert_a4_def(M) || {'Map', M} <- DefMaps],
        case WordDefs0 of
            [] -> {error, no_words_defined};
            _ ->
                AllNames = lists:usort([N || {N, _, _, _} <- WordDefs0]),
                WordDefs = retranslate_bodies(WordDefs0, AllNames),
                MergedDefs = merge_multi_clause(WordDefs),
                af_ring1:compile_module(ModAtom, MergedDefs, [{mode, native}])
        end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

compile_file_via_a4_compiler(FilePath, ModName) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            compile_via_a4_compiler(Content, FilePath, ModName);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% Callable from compiled A4 via xcall: takes stack [DefMaps, ModName]
%% Converts A4 compiler output maps to Ring 0 tuple format and compiles via Ring 1.
compile_from_a4_defs([{'List', DefMaps}, {'String', ModName} | Rest]) ->
    ModAtom = list_to_atom("af_r2_" ++ binary_to_list(ModName)),
    WordDefs0 = [convert_a4_def(M) || {'Map', M} <- DefMaps],
    case WordDefs0 of
        [] -> [{'Bool', false} | Rest];
        _ ->
            AllNames = lists:usort([N || {N, _, _, _} <- WordDefs0]),
            WordDefs = retranslate_bodies(WordDefs0, AllNames),
            MergedDefs = merge_multi_clause(WordDefs),
            case af_ring1:compile_module(ModAtom, MergedDefs, [{mode, native}]) of
                {ok, _} -> [{'Bool', true} | Rest];
                _ -> [{'Bool', false} | Rest]
            end
    end.

%% Convert a single word definition map to {Name, SigIn, SigOut, Body} tuple
convert_a4_def(Map) ->
    Name = get_str(Map, <<"name">>),
    SigIn = convert_sig(get_list(Map, <<"sig_in">>)),
    SigOut = convert_sig(get_list(Map, <<"sig_out">>)),
    Body = convert_body(get_list(Map, <<"body">>)),
    {Name, SigIn, SigOut, Body}.

%% Extract string value from map with String-tagged keys
get_str(Map, Key) ->
    {'String', Val} = maps:get({'String', Key}, Map),
    binary_to_list(Val).

get_list(Map, Key) ->
    {'List', Val} = maps:get({'String', Key}, Map),
    Val.

%% Convert signature list: ["Int", "Bool"] -> [Int, Bool] (atoms)
convert_sig(Items) ->
    [convert_sig_item(I) || I <- Items].

convert_sig_item({'String', <<"True">>}) -> {'Bool', true};
convert_sig_item({'String', <<"true">>}) -> {'Bool', true};
convert_sig_item({'String', <<"False">>}) -> {'Bool', false};
convert_sig_item({'String', <<"false">>}) -> {'Bool', false};
convert_sig_item({'String', Name}) ->
    case try_parse_int_bin(Name) of
        {ok, N} -> {'Int', N};
        error -> binary_to_atom(Name, utf8)
    end.

try_parse_int_bin(Bin) ->
    try {ok, binary_to_integer(Bin)}
    catch _:_ -> error
    end.

%% Convert body instruction list
convert_body(Items) ->
    lists:flatmap(fun convert_inst/1, Items).

convert_inst({'Map', M}) ->
    {'String', Op} = maps:get({'String', <<"op">>}, M),
    convert_inst_by_op(binary_to_list(Op), M).

convert_inst_by_op("lit", M) ->
    {'Map', ValMap} = maps:get({'String', <<"val">>}, M),
    {'String', TypeBin} = maps:get({'String', <<"type">>}, ValMap),
    Type = binary_to_atom(TypeBin, utf8),
    case Type of
        'Int' ->
            {'Int', Val} = maps:get({'String', <<"value">>}, ValMap),
            [{lit, {'Int', Val}}];
        'Bool' ->
            {'Bool', Val} = maps:get({'String', <<"value">>}, ValMap),
            [{lit, {'Bool', Val}}];
        'String' ->
            {'String', Val} = maps:get({'String', <<"value">>}, ValMap),
            [{lit, {'String', Val}}]
    end;
convert_inst_by_op("apply_impl", M) ->
    {'String', Name} = maps:get({'String', <<"name">>}, M),
    [{apply_impl, binary_to_list(Name)}];
convert_inst_by_op("select_clause", M) ->
    {'List', Clauses} = maps:get({'String', <<"clauses">>}, M),
    ConvertedClauses = [convert_clause(C) || C <- Clauses],
    [{lit, {'List', ConvertedClauses}}, select_clause];
convert_inst_by_op("product_new", M) ->
    {'String', TypeName} = maps:get({'String', <<"type_name">>}, M),
    {'List', FieldsList} = maps:get({'String', <<"fields">>}, M),
    Fields = [binary_to_atom(F, utf8) || {'String', F} <- FieldsList],
    [{product_new, binary_to_atom(TypeName, utf8), Fields}];
convert_inst_by_op("product_get", M) ->
    {'String', Field} = maps:get({'String', <<"field">>}, M),
    [{product_get, binary_to_atom(Field, utf8)}];
convert_inst_by_op("product_set", M) ->
    {'String', Field} = maps:get({'String', <<"field">>}, M),
    [{product_set, binary_to_atom(Field, utf8)}];
convert_inst_by_op(Op, _M) ->
    [list_to_atom(Op)].

convert_clause({'Map', M}) ->
    SigIn = convert_sig(get_list(M, <<"sig_in">>)),
    Body = convert_body(get_list(M, <<"body">>)),
    {SigIn, Body}.

compile_file_selfhosted(FilePath, ModName) ->
    compile_file_selfhosted(FilePath, ModName, []).

compile_file_selfhosted(FilePath, ModName, _Opts) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            compile_selfhosted(Content, FilePath, ModName);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% Re-translate bodies with full word name list for correct call resolution.
retranslate_bodies(WordDefs, AllNames) ->
    [{Name, SigIn, SigOut, retranslate_body(Body, AllNames)}
     || {Name, SigIn, SigOut, Body} <- WordDefs].

retranslate_body(Body, AllNames) ->
    lists:map(fun
        ({apply_impl, OpName}) ->
            case lists:member(OpName, AllNames) of
                true -> {call, OpName};
                false -> {apply_impl, OpName}
            end;
        ({lit, {'List', Clauses}}) when is_list(Clauses) ->
            %% Recurse into select_clause bodies
            NewClauses = [{SigIn, retranslate_body(ClauseBody, AllNames)}
                          || {SigIn, ClauseBody} <- Clauses],
            {lit, {'List', NewClauses}};
        (Other) -> Other
    end, Body).

%%% === Word Translation ===

%% Translate a compiled word definition to Ring 0 format.
%% WordDef = {Name, SigIn, SigOut, Body}
%% Body = [#operation{}] (late-binding thunks from af_type_compiler)
%% Returns {Name, SigIn, SigOut, R0Body}
translate_word({Name, SigIn, SigOut, Body}, WordNames) ->
    R0Body = translate_body(Body, WordNames),
    {Name, SigIn, SigOut, R0Body}.

%% Translate a word body (list of #operation{}) to Ring 0 instructions.
translate_body(Ops, WordNames) ->
    lists:flatmap(fun(Op) -> translate_op(Op, WordNames) end, Ops).

%%% === Operation Translation ===

%% Translate a single #operation{} to Ring 0 instruction(s).
translate_op(#operation{name = Name, source = quoted_string}, _WN) ->
    [{lit, {'String', list_to_binary(Name)}}];

translate_op(#operation{name = Name}, WN) ->
    case translate_primitive(Name) of
        {ok, Insts} -> Insts;
        not_found ->
            case try_literal(Name) of
                {ok, Value} -> [{lit, Value}];
                not_found ->
                    case lists:member(Name, WN) of
                        true -> [{call, Name}];
                        false -> [{apply_impl, Name}]
                    end
            end
    end.

%% Map A4 operation names to Ring 0 instructions.
translate_primitive("dup")   -> {ok, [dup]};
translate_primitive("drop")  -> {ok, [drop]};
translate_primitive("swap")  -> {ok, [swap]};
translate_primitive("rot")   -> {ok, [to_r, swap, from_r, swap]};
translate_primitive("over")  -> {ok, [to_r, dup, from_r, swap]};
translate_primitive("2dup")  -> {ok, [to_r, dup, from_r, swap,
                                     to_r, dup, from_r, swap]};
%% Arithmetic
translate_primitive("+")     -> {ok, [add]};
translate_primitive("-")     -> {ok, [sub]};
translate_primitive("*")     -> {ok, [mul]};
translate_primitive("/")     -> {ok, [divop]};
translate_primitive("mod")   -> {ok, [modop]};
%% Comparison
translate_primitive("==")    -> {ok, [eq]};
translate_primitive("!=")    -> {ok, [eq, not_op]};
translate_primitive("<")     -> {ok, [lt]};
translate_primitive(">")     -> {ok, [swap, lt]};
translate_primitive("<=")    -> {ok, [swap, lt, not_op]};
translate_primitive(">=")    -> {ok, [lt, not_op]};
%% Logic
translate_primitive("not")   -> {ok, [not_op]};
translate_primitive("and")   -> {ok, [and_op]};
translate_primitive("or")    -> {ok, [or_op]};
%% List
translate_primitive("nil")   -> {ok, [nil]};
translate_primitive("cons")  -> {ok, [cons]};
translate_primitive("head")  -> {ok, [head]};
translate_primitive("tail")  -> {ok, [tail]};
translate_primitive("length")    -> {ok, [generic_len]};
translate_primitive("append")    -> {ok, [list_append]};
translate_primitive("reverse")   -> {ok, [list_reverse]};
translate_primitive("nth")       -> {ok, [list_nth]};
translate_primitive("last")      -> {ok, [list_last]};
translate_primitive("take")      -> {ok, [list_take]};
translate_primitive("empty?")    -> {ok, [list_empty]};
translate_primitive("contains?") -> {ok, [list_contains]};
translate_primitive("flatten")   -> {ok, [list_flatten]};
translate_primitive("zip")       -> {ok, [list_zip]};
%% Map
translate_primitive("map-new")    -> {ok, [map_new]};
translate_primitive("map-put")    -> {ok, [map_put]};
translate_primitive("map-get")    -> {ok, [map_get]};
translate_primitive("map-delete") -> {ok, [map_delete]};
translate_primitive("map-keys")   -> {ok, [map_keys]};
translate_primitive("map-has?")   -> {ok, [map_has]};
translate_primitive("map-values") -> {ok, [map_values]};
translate_primitive("map-size")   -> {ok, [map_size]};
translate_primitive("map-merge")  -> {ok, [map_merge]};
translate_primitive("map-get-or") -> {ok, [map_get_or]};
%% String
translate_primitive("concat")      -> {ok, [str_concat]};
translate_primitive("split")       -> {ok, [str_split]};
translate_primitive("contains")    -> {ok, [str_contains]};
translate_primitive("starts-with") -> {ok, [str_starts_with]};
translate_primitive("ends-with")   -> {ok, [str_ends_with]};
translate_primitive("trim")        -> {ok, [str_trim]};
translate_primitive("to-upper")    -> {ok, [str_upper]};
translate_primitive("to-lower")    -> {ok, [str_lower]};
translate_primitive("substring")   -> {ok, [str_substring]};
translate_primitive("replace")     -> {ok, [str_replace]};
translate_primitive("str-nth")     -> {ok, [str_nth]};
translate_primitive("str-byte-at") -> {ok, [str_byte_at]};
%% Conversion
translate_primitive("to-string")  -> {ok, [to_string]};
translate_primitive("to-int")     -> {ok, [to_int]};
translate_primitive("to-float")   -> {ok, [to_float]};
translate_primitive("to-atom")    -> {ok, [to_atom]};
%% Tuple
translate_primitive("make-tuple") -> {ok, [tuple_make]};
translate_primitive("from-tuple") -> {ok, [tuple_to_list]};
translate_primitive("tuple-size") -> {ok, [tuple_size_op]};
translate_primitive("ok-tuple")   -> {ok, [ok_tuple]};
translate_primitive("error-tuple") -> {ok, [error_tuple]};
translate_primitive("is-ok")      -> {ok, [is_ok]};
translate_primitive("unwrap-ok")  -> {ok, [unwrap_ok]};
%% I/O
translate_primitive("print")      -> {ok, [print_tos]};
translate_primitive("stack")      -> {ok, [print_stack]};
translate_primitive("assert")     -> {ok, [assert_true]};
translate_primitive("assert-eq")  -> {ok, [assert_eq]};
%% File
translate_primitive("read-file")   -> {ok, [file_read]};
translate_primitive("write-file")  -> {ok, [file_write]};
translate_primitive("file-exists?") -> {ok, [file_exists]};
%% FFI
translate_primitive("erlang-apply")  -> {ok, [{apply_impl, "erlang-apply"}]};
translate_primitive("erlang-apply0") -> {ok, [{apply_impl, "erlang-apply0"}]};
translate_primitive("erlang-call")   -> {ok, [{apply_impl, "erlang-call"}]};
translate_primitive("erlang-call0")  -> {ok, [{apply_impl, "erlang-call0"}]};
translate_primitive("erlang-new")    -> {ok, [{apply_impl, "erlang-new"}]};
%% Actor
translate_primitive("spawn")   -> {ok, [spawn_actor]};
translate_primitive("send")    -> {ok, [send_msg]};
translate_primitive("!")       -> {ok, [send_msg]};
translate_primitive("receive") -> {ok, [receive_msg]};
translate_primitive("receive-timeout") -> {ok, [receive_timeout]};
translate_primitive(_)       -> not_found.

%% Try to parse a token value as a literal.
try_literal(Name) ->
    case catch list_to_integer(Name) of
        N when is_integer(N) -> {ok, {'Int', N}};
        _ ->
            case catch list_to_float(Name) of
                F when is_float(F) -> {ok, {'Float', F}};
                _ ->
                    case Name of
                        "true"  -> {ok, {'Bool', true}};
                        "false" -> {ok, {'Bool', false}};
                        _       -> not_found
                    end
            end
    end.

%%% === Word Discovery ===

%% Find all user-defined compiled words across all types.
%% Returns [{Name, SigIn, SigOut, Body}]
find_all_compiled_words() ->
    AllTypes = af_type:all_types(),
    lists:flatmap(fun(#af_type{ops = Ops}) ->
        maps:fold(fun(_Name, OpList, Acc) ->
            lists:filtermap(fun
                (#operation{name = N, sig_in = SI, sig_out = SO,
                            source = {compiled, Body}}) ->
                    {true, {N, SI, SO, Body}};
                (_) -> false
            end, OpList) ++ Acc
        end, [], Ops)
    end, AllTypes).

%%% === Multi-Clause Merging ===

%% Group same-name word defs and merge into select_clause bodies.
merge_multi_clause(WordDefs) ->
    Grouped = group_by_name(WordDefs),
    lists:flatmap(fun({_Name, Defs}) ->
        case Defs of
            [Single] -> [Single];
            Multiple ->
                %% Merge into a single word with select_clause body
                Clauses = [{SigIn, Body} || {_, SigIn, _, Body} <- Multiple],
                {Name, _, SigOut, _} = hd(Multiple),
                MergedBody = [{lit, {'List', Clauses}}, select_clause],
                [{Name, [], SigOut, MergedBody}]
        end
    end, Grouped).

group_by_name(WordDefs) ->
    lists:foldl(fun({Name, _, _, _} = WD, Acc) ->
        case lists:keyfind(Name, 1, Acc) of
            false -> [{Name, [WD]} | Acc];
            {Name, Existing} ->
                lists:keyreplace(Name, 1, Acc, {Name, Existing ++ [WD]})
        end
    end, [], WordDefs).

%%% === Helpers ===

to_str(B) when is_binary(B) -> binary_to_list(B);
to_str(L) when is_list(L) -> L;
to_str(A) when is_atom(A) -> atom_to_list(A).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8).

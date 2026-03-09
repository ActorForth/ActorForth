%% af_ring2.erl -- Ring 2: A4 Compiler targeting Ring 0/Ring 1
%%
%% Compiles A4 source into Ring 0 instruction sequences, then uses
%% Ring 1 to generate BEAM modules. During bootstrap, delegates
%% parsing and word definition to the existing Erlang infrastructure,
%% then translates compiled word bodies to Ring 0 instructions.
%%
%% Usage:
%%   {ok, Mod} = af_ring2:compile(": double Int -> Int ; dup + .", "test", "my_mod").
%%   Mod:double([{'Int', 21}]).  %% => [{'Int', 42}]

-module(af_ring2).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([
    compile/3,
    compile_file/2,
    compile_file/3,
    translate_word/2,
    translate_body/2
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
                %% Phase 4: Compile via Ring 1
                af_ring1:compile_module(ModAtom, MergedDefs)
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
%% List
translate_primitive("nil")   -> {ok, [nil]};
translate_primitive("cons")  -> {ok, [cons]};
translate_primitive("head")  -> {ok, [head]};
translate_primitive("tail")  -> {ok, [tail]};
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

%% af_r0_compiler.erl -- Self-hosted A4 word compiler using Ring 0 data model
%%
%% Compiles A4 token streams directly into Ring 0 word definitions,
%% bypassing af_interpreter.erl and af_type_compiler.erl entirely.
%% This is the "dispatch loop" and "word compiler" for self-hosting.
%%
%% The compiler processes token streams looking for word definitions:
%%   : name InputTypes -> OutputTypes ; body .
%%
%% And produces Ring 0 word definitions:
%%   [{Name, SigIn, SigOut, R0Body}]
%%
%% Usage:
%%   Tokens = af_r0_parser:parse(Source, File),
%%   WordDefs = af_r0_compiler:compile_tokens(Tokens).

-module(af_r0_compiler).

-include("af_type.hrl").

-export([compile_tokens/1]).

%%% === Public API ===

%% Compile a list of token maps into Ring 0 word definitions.
%% Returns [{Name, SigIn, SigOut, R0Body}]
compile_tokens(Tokens) ->
    {WordDefs, _Rest} = process_tokens(Tokens, []),
    WordDefs.

%%% === Token Processing ===

%% Process top-level tokens, looking for word definitions (: ... .)
process_tokens([], Acc) ->
    {lists:reverse(Acc), []};
process_tokens([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<":">>, false} ->
            %% Start of word definition
            {WordDef, Rest1} = parse_word_def(Rest),
            process_tokens(Rest1, [WordDef | Acc]);
        {<<"type">>, false} ->
            %% Product type definition
            {TypeDefs, Rest1} = parse_product_type(Rest),
            process_tokens(Rest1, TypeDefs ++ Acc);
        {<<"load">>, false} ->
            %% Load and compile another file
            case Rest of
                [PathTok | Rest1] ->
                    Path = maps:get(value, PathTok),
                    case maps:get(quoted, PathTok) of
                        true ->
                            LoadedDefs = load_file(Path),
                            process_tokens(Rest1, LoadedDefs ++ Acc);
                        false ->
                            %% Non-quoted load path (treat as filename)
                            LoadedDefs = load_file(Path),
                            process_tokens(Rest1, LoadedDefs ++ Acc)
                    end;
                [] ->
                    {lists:reverse(Acc), []}
            end;
        _ ->
            %% Skip non-definition tokens (bare expressions)
            process_tokens(Rest, Acc)
    end.

%%% === Word Definition Parsing ===

%% Parse: name InputTypes -> OutputTypes ; body .
%% Returns {{Name, SigIn, SigOut, R0Body}, RemainingTokens}
parse_word_def([NameTok | Rest]) ->
    Name = binary_to_list(maps:get(value, NameTok)),
    {SigIn, Rest1} = parse_sig_in(Rest, []),
    {SigOut, Rest2} = parse_sig_out(Rest1, []),
    {Body, Rest3} = parse_body(Rest2, []),
    WordNames = [Name],  %% Will be enriched later by Ring 2
    R0Body = translate_body(Body, WordNames),
    %% SigIn: keep as TOS-first (last source token accumulated first)
    %% SigOut: reverse to source order for display
    {{Name, SigIn, lists:reverse(SigOut), R0Body}, Rest3}.

%%% === Signature Parsing ===

%% Parse input type signature until we hit "->" or ";"
parse_sig_in([], Acc) -> {Acc, []};
parse_sig_in([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<"->">>, false} ->
            {Acc, Rest};
        {<<";">>, false} ->
            %% No output signature, go straight to body
            {Acc, [Token | Rest]};
        _ ->
            Entry = parse_sig_entry(Value, Quoted, Rest, Acc),
            case Entry of
                {SigItem, NewRest} ->
                    parse_sig_in(NewRest, [SigItem | Acc]);
                {SigItem, NewAcc, NewRest} ->
                    parse_sig_in(NewRest, [SigItem | NewAcc])
            end
    end.

%% Parse output type signature until we hit ";"
parse_sig_out([], Acc) -> {Acc, []};
parse_sig_out([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<";">>, false} ->
            {Acc, Rest};
        _ ->
            Entry = parse_sig_entry(Value, Quoted, Rest, Acc),
            case Entry of
                {SigItem, NewRest} ->
                    parse_sig_out(NewRest, [SigItem | Acc]);
                {SigItem, _NewAcc, NewRest} ->
                    parse_sig_out(NewRest, [SigItem | Acc])
            end
    end.

%% Parse a single signature entry.
%% Could be a type name (atom) or a value constraint (literal + type).
parse_sig_entry(Value, false, Rest, _Acc) ->
    %% Try as integer literal (value constraint)
    case try_parse_int(Value) of
        {ok, N} ->
            %% Next token should be the type name, or -> (infer Int)
            case Rest of
                [TypeTok | Rest1] ->
                    TypeVal = maps:get(value, TypeTok),
                    case TypeVal of
                        <<"->">> -> {{'Int', N}, Rest};  %% Infer Int, don't consume ->
                        <<";">> -> {{'Int', N}, Rest};    %% Infer Int, don't consume ;
                        _ ->
                            TypeName = binary_to_atom(TypeVal, utf8),
                            {{TypeName, N}, Rest1}
                    end;
                [] -> {{'Int', N}, Rest}
            end;
        error ->
            %% Try as boolean literal (both A4 convention True/False and lowercase)
            case Value of
                V when V =:= <<"true">>; V =:= <<"True">> ->
                    parse_bool_constraint(true, Rest);
                V when V =:= <<"false">>; V =:= <<"False">> ->
                    parse_bool_constraint(false, Rest);
                _ ->
                    %% Regular type name
                    {binary_to_atom(Value, utf8), Rest}
            end
    end;
parse_sig_entry(Value, true, Rest, _Acc) ->
    %% Quoted string in signature — value constraint (keep as binary)
    case Rest of
        [TypeTok | Rest1] ->
            TypeVal = maps:get(value, TypeTok),
            case TypeVal of
                <<"->">> -> {{'String', Value}, Rest};
                <<";">> -> {{'String', Value}, Rest};
                _ ->
                    TypeName = binary_to_atom(TypeVal, utf8),
                    {{TypeName, Value}, Rest1}
            end;
        [] ->
            {{'String', Value}, Rest}
    end.

%% Helper for Bool value constraints with lookahead for -> or ;
parse_bool_constraint(BoolVal, Rest) ->
    case Rest of
        [TypeTok | Rest1] ->
            TypeVal = maps:get(value, TypeTok),
            case TypeVal of
                <<"->">> -> {{'Bool', BoolVal}, Rest};
                <<";">> -> {{'Bool', BoolVal}, Rest};
                _ -> {{binary_to_atom(TypeVal, utf8), BoolVal}, Rest1}
            end;
        [] -> {{'Bool', BoolVal}, Rest}
    end.

%%% === Body Parsing ===

%% Parse body tokens until we hit "." (end of word)
%% Detects sub-clause syntax: `: value -> result ;` inside body
parse_body([], Acc) -> {lists:reverse(Acc), []};
parse_body([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<".">>, false} ->
            {lists:reverse(Acc), Rest};
        {<<":">>, false} ->
            %% Sub-clause detected — collect all sub-clauses
            {SubClauses, Rest1} = parse_sub_clauses([Token | Rest]),
            %% Return sub-clauses as a special marker
            {lists:reverse(Acc) ++ [{sub_clauses, SubClauses}], Rest1};
        _ ->
            parse_body(Rest, [{Value, Quoted} | Acc])
    end.

%% Parse sub-clauses until we hit "." at the word level.
%% Each sub-clause: : sig_in -> sig_out ; body
%% The final "." ends the entire word (including all sub-clauses).
parse_sub_clauses(Tokens) ->
    parse_sub_clauses(Tokens, []).

parse_sub_clauses([], Acc) -> {lists:reverse(Acc), []};
parse_sub_clauses([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<".">>, false} ->
            %% End of word — return all collected sub-clauses
            {lists:reverse(Acc), Rest};
        {<<":">>, false} ->
            %% Parse one sub-clause
            {SubClause, Rest1} = parse_one_sub_clause(Rest),
            parse_sub_clauses(Rest1, [SubClause | Acc]);
        _ ->
            %% Skip other tokens (shouldn't happen in well-formed source)
            parse_sub_clauses(Rest, Acc)
    end.

%% Parse a single sub-clause: sig_in -> sig_out ; body (ends at ; or next :)
parse_one_sub_clause(Tokens) ->
    {SigIn, Rest1} = parse_sig_in(Tokens, []),
    %% sig_out parsing may or may not be present
    %% If we hit ; directly, there's no explicit -> output
    {SigOut, Rest2} = parse_sig_out(Rest1, []),
    {Body, Rest3} = parse_sub_body(Rest2, []),
    %% SigIn: keep as TOS-first
    {{SigIn, lists:reverse(SigOut), Body}, Rest3}.

%% Parse sub-clause body until ";" (sub-clause end) or "." (word end)
parse_sub_body([], Acc) -> {lists:reverse(Acc), []};
parse_sub_body([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<".">>, false} ->
            %% End of word — put "." back for parent parser
            {lists:reverse(Acc), [Token | Rest]};
        {<<":">>, false} ->
            %% Start of next sub-clause — put ":" back for parent parser
            {lists:reverse(Acc), [Token | Rest]};
        _ ->
            parse_sub_body(Rest, [{Value, Quoted} | Acc])
    end.

%%% === Body Translation to Ring 0 ===

%% Translate parsed body tokens to Ring 0 instructions.
translate_body(BodyTokens, WordNames) ->
    lists:flatmap(fun
        ({sub_clauses, Clauses}) ->
            translate_sub_clauses(Clauses, WordNames);
        ({Value, Quoted}) ->
            translate_token(Value, Quoted, WordNames)
    end, BodyTokens).

%% Translate sub-clauses into select_clause Ring 0 instructions.
translate_sub_clauses(Clauses, WordNames) ->
    R0Clauses = lists:map(fun({SigIn, _SigOut, Body}) ->
        R0Body = translate_body(Body, WordNames),
        R0SigIn = translate_sig_to_r0(SigIn),
        {R0SigIn, R0Body}
    end, Clauses),
    [{lit, {'List', R0Clauses}}, select_clause].

%% Translate signature entries to Ring 0 match_sig format.
translate_sig_to_r0(SigIn) ->
    lists:map(fun
        ({Type, Value}) -> {Type, Value};  %% Value constraint
        (Type) when is_atom(Type) -> Type  %% Type match
    end, SigIn).

translate_token(Value, true, _WN) ->
    %% Quoted string literal
    [{lit, {'String', Value}}];
translate_token(Value, false, WN) ->
    Name = binary_to_list(Value),
    case translate_primitive(Name) of
        {ok, Insts} -> Insts;
        not_found ->
            case try_literal(Name) of
                {ok, LitVal} -> [{lit, LitVal}];
                not_found ->
                    case lists:member(Name, WN) of
                        true -> [{call, Name}];
                        false -> [{apply_impl, Name}]
                    end
            end
    end.

%%% === Primitive Translation ===
%%% (Same as af_ring2.erl — maps A4 op names to Ring 0 instructions)

translate_primitive("dup")   -> {ok, [dup]};
translate_primitive("drop")  -> {ok, [drop]};
translate_primitive("pop")   -> {ok, [drop]};
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
%% Logic
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
translate_primitive("list-to-raw") -> {ok, [list_to_raw]};
translate_primitive("deep-to-raw") -> {ok, [deep_to_raw]};
translate_primitive("from-tuple") -> {ok, [tuple_to_list]};
translate_primitive("tuple-size") -> {ok, [tuple_size_op]};
translate_primitive("ok-tuple")   -> {ok, [ok_tuple]};
translate_primitive("error-tuple") -> {ok, [error_tuple]};
translate_primitive("is-ok")      -> {ok, [is_ok]};
translate_primitive("unwrap-ok")  -> {ok, [unwrap_ok]};
%% I/O
translate_primitive("print")      -> {ok, [print_tos]};
translate_primitive(".")          -> {ok, [dot]};
translate_primitive("stack")      -> {ok, [print_stack]};
translate_primitive(".s")         -> {ok, [print_stack]};
translate_primitive("assert")     -> {ok, [assert_true]};
translate_primitive("assert-eq")  -> {ok, [assert_eq]};
%% File
translate_primitive("read-file")   -> {ok, [file_read]};
translate_primitive("write-file")  -> {ok, [file_write]};
translate_primitive("file-exists?") -> {ok, [file_exists]};
%% FFI - these stay as apply_impl since they need the full runtime
translate_primitive("erlang-apply")  -> {ok, [{apply_impl, "erlang-apply"}]};
translate_primitive("erlang-apply0") -> {ok, [{apply_impl, "erlang-apply0"}]};
translate_primitive("erlang-call")   -> {ok, [{apply_impl, "erlang-call"}]};
translate_primitive("erlang-call0")  -> {ok, [{apply_impl, "erlang-call0"}]};
translate_primitive("erlang-new")    -> {ok, [{apply_impl, "erlang-new"}]};
translate_primitive("xcall")         -> {ok, [{apply_impl, "xcall"}]};
%% Actor primitives
translate_primitive("spawn")   -> {ok, [spawn_actor]};
translate_primitive("send")    -> {ok, [send_msg]};
translate_primitive("!")       -> {ok, [send_msg]};
translate_primitive("receive") -> {ok, [receive_msg]};
translate_primitive("receive-timeout") -> {ok, [receive_timeout]};
translate_primitive(_)       -> not_found.

%%% === Literal Parsing ===

try_literal(Name) ->
    case try_parse_int_str(Name) of
        {ok, N} -> {ok, {'Int', N}};
        error ->
            case try_parse_float(Name) of
                {ok, F} -> {ok, {'Float', F}};
                error ->
                    case Name of
                        "true"  -> {ok, {'Bool', true}};
                        "false" -> {ok, {'Bool', false}};
                        _       -> not_found
                    end
            end
    end.

try_parse_int(Bin) when is_binary(Bin) ->
    try {ok, binary_to_integer(Bin)}
    catch _:_ -> error
    end.

try_parse_int_str(Str) when is_list(Str) ->
    try {ok, list_to_integer(Str)}
    catch _:_ -> error
    end.

try_parse_float(Str) when is_list(Str) ->
    try {ok, list_to_float(Str)}
    catch _:_ -> error
    end.

%%% === File Loading ===

load_file(Path) when is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Tokens = af_r0_parser:parse(Content, Path),
            compile_tokens(Tokens);
        {error, _} ->
            %% Try with .a4 extension
            Path1 = <<Path/binary, ".a4">>,
            case file:read_file(Path1) of
                {ok, Content} ->
                    Tokens = af_r0_parser:parse(Content, Path1),
                    compile_tokens(Tokens);
                {error, _} -> []
            end
    end.

%%% === Product Type Compilation ===
%%% Generates constructor, getter, and setter words for product types.
%%% `type Point x Int y Int .` generates:
%%%   point (constructor), x (getter), y (getter), x! (setter), y! (setter)

%% Parse: type Name field1 Type1 field2 Type2 ... .
%% Returns {[WordDef, ...], RemainingTokens}
parse_product_type([NameTok | Rest]) ->
    TypeName = binary_to_atom(maps:get(value, NameTok), utf8),
    Constructor = string:lowercase(binary_to_list(maps:get(value, NameTok))),
    {Fields, Rest1} = parse_type_fields(Rest, []),
    FieldTypes = [T || {_, T} <- Fields],
    NumFields = length(Fields),
    %% Flat-tuple layout: field i (0-indexed) lives at tuple position
    %% i+2 (position 1 is the type atom). Resolve field positions at
    %% compile time so Ring 0 / Ring 1 opcodes carry integers, not
    %% field names.
    SigIn = [binary_to_atom(T, utf8) || T <- lists:reverse(FieldTypes)],
    ConstructorDef = {Constructor, SigIn, [TypeName],
                      [{product_new, TypeName, NumFields}]},
    {Getters, _} = lists:mapfoldl(fun({F, T}, Pos) ->
        FName = binary_to_list(F),
        FType = binary_to_atom(T, utf8),
        Def = {FName, [TypeName], [FType, TypeName],
               [{product_get, Pos}]},
        {Def, Pos + 1}
    end, 2, Fields),
    {Setters, _} = lists:mapfoldl(fun({F, T}, Pos) ->
        FName = binary_to_list(F) ++ "!",
        FType = binary_to_atom(T, utf8),
        Def = {FName, [FType, TypeName], [TypeName],
               [{product_set, Pos}]},
        {Def, Pos + 1}
    end, 2, Fields),
    %% Register the type in the ETS registry so downstream stages
    %% (Ring 1 head-pattern generation in particular) can recognise
    %% it as a product type. Without this step the self-hosted
    %% pipeline emits a 2-tuple pattern against a flat-tuple
    %% instance and the generated BEAM module fails to match its
    %% own product arguments.
    register_product_type_meta(TypeName, Fields),
    {lists:reverse([ConstructorDef] ++ Getters ++ Setters), Rest1}.

register_product_type_meta(TypeName, Fields) ->
    FieldSpecs = [
        {list_to_atom(binary_to_list(F)),
         binary_to_atom(T, utf8)}
        || {F, T} <- Fields
    ],
    af_type:init(),
    af_type:register_type(
        #af_type{name = TypeName, fields = FieldSpecs}),
    ok.

parse_type_fields([], Acc) -> {lists:reverse(Acc), []};
parse_type_fields([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    case Value of
        <<".">> ->
            {lists:reverse(Acc), Rest};
        _ ->
            %% field name followed by type
            case Rest of
                [TypeTok | Rest1] ->
                    TypeName = maps:get(value, TypeTok),
                    parse_type_fields(Rest1, [{Value, TypeName} | Acc]);
                [] ->
                    {lists:reverse(Acc), []}
            end
    end.

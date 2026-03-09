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
    {{Name, lists:reverse(SigIn), lists:reverse(SigOut), R0Body}, Rest3}.

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
            %% Next token should be the type name
            case Rest of
                [TypeTok | Rest1] ->
                    TypeName = binary_to_atom(maps:get(value, TypeTok), utf8),
                    {{TypeName, N}, Rest1};
                [] ->
                    %% Bare integer at end — treat as type
                    {binary_to_atom(Value, utf8), Rest}
            end;
        error ->
            %% Try as boolean literal
            case Value of
                <<"true">> ->
                    case Rest of
                        [TypeTok | Rest1] ->
                            TypeName = binary_to_atom(maps:get(value, TypeTok), utf8),
                            {{TypeName, true}, Rest1};
                        [] -> {'Bool', Rest}
                    end;
                <<"false">> ->
                    case Rest of
                        [TypeTok | Rest1] ->
                            TypeName = binary_to_atom(maps:get(value, TypeTok), utf8),
                            {{TypeName, false}, Rest1};
                        [] -> {'Bool', Rest}
                    end;
                _ ->
                    %% Regular type name
                    {binary_to_atom(Value, utf8), Rest}
            end
    end;
parse_sig_entry(Value, true, Rest, _Acc) ->
    %% Quoted string in signature — value constraint
    case Rest of
        [TypeTok | Rest1] ->
            TypeName = binary_to_atom(maps:get(value, TypeTok), utf8),
            {{TypeName, binary_to_list(Value)}, Rest1};
        [] ->
            {'String', Rest}
    end.

%%% === Body Parsing ===

%% Parse body tokens until we hit "." (end of word)
parse_body([], Acc) -> {lists:reverse(Acc), []};
parse_body([Token | Rest], Acc) ->
    Value = maps:get(value, Token),
    Quoted = maps:get(quoted, Token),
    case {Value, Quoted} of
        {<<".">>, false} ->
            {lists:reverse(Acc), Rest};
        _ ->
            parse_body(Rest, [{Value, Quoted} | Acc])
    end.

%%% === Body Translation to Ring 0 ===

%% Translate parsed body tokens to Ring 0 instructions.
translate_body(BodyTokens, WordNames) ->
    lists:flatmap(fun({Value, Quoted}) ->
        translate_token(Value, Quoted, WordNames)
    end, BodyTokens).

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
%% Map
translate_primitive("map-new")    -> {ok, [map_new]};
translate_primitive("map-put")    -> {ok, [map_put]};
translate_primitive("map-get")    -> {ok, [map_get]};
translate_primitive("map-delete") -> {ok, [map_delete]};
translate_primitive("map-keys")   -> {ok, [map_keys]};
translate_primitive("map-has?")   -> {ok, [map_has]};
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

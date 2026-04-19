-module(af_type_check).

-include("operation.hrl").
-include("af_type.hrl").

-export([check_word/4, check_word/5,
         infer_stack/2, infer_stack/3,
         is_type_variable/1]).

%% Check a word definition's body against its declared signatures.
%% Returns ok | {error, Reason}.
%%
%% SigIn/SigOut: declared type signatures (TOS-first ordering).
%% Body: list of #operation{} with name fields (as produced by resolve_compile_token).
%%
%% Strategy: start with a type stack matching SigIn, apply each body token's
%% stack effect, then verify the result matches SigOut.

check_word(Name, SigIn, SigOut, Body) ->
    check_word(Name, SigIn, SigOut, Body, #{}).

check_word(Name, SigIn, SigOut, Body, Locals) ->
    InitStack = SigIn,
    case infer_stack(Body, InitStack, Locals) of
        {ok, ResultStack} ->
            case match_output(ResultStack, SigOut) of
                true -> ok;
                false ->
                    {error, {type_mismatch, Name,
                        #{expected => SigOut, actual => ResultStack}}}
            end;
        {error, _} = Err ->
            Err
    end.

%% Infer the type stack after applying a sequence of operations.
%% Returns {ok, ResultTypeStack} | {error, Reason}.
infer_stack(Body, Stack) ->
    infer_stack(Body, Stack, #{}).

infer_stack([], Stack, _Locals) -> {ok, Stack};
infer_stack([#operation{name = Name, source = Source} | Rest], Stack, Locals) ->
    case resolve_stack_effect(Name, Source, Stack, Locals) of
        {ok, NewStack} ->
            infer_stack(Rest, NewStack, Locals);
        {error, _} = Err ->
            Err
    end.

%% Determine the stack effect of a named operation given the current type
%% stack, the operation's source classification, and the virtual locals map.
%% Returns {ok, NewStack} | {error, Reason}.

%% Bracket literal `]`: collect everything back to the nearest ListBuilder
%% sentinel on the type stack, pop the sentinel, and push a single List.
resolve_stack_effect("]", _Source, Stack, _Locals) ->
    case collect_to_list_builder(Stack, 0) of
        {ok, RemainingStack, _Count} ->
            {ok, ['List' | RemainingStack]};
        no_builder ->
            {error, {unbalanced_bracket, "]", Stack}}
    end;

resolve_stack_effect(Name, Source, Stack, Locals) ->
    %% 1. Quoted string literal — always pushes a String.
    case Source of
        quoted_string ->
            {ok, ['String' | Stack]};
        _ ->
            resolve_non_quoted(Name, Stack, Locals)
    end.

resolve_non_quoted(Name, Stack, Locals) ->
    %% 2. Virtual locals lookup — auto-field bindings from product-type inputs.
    case maps:find(Name, Locals) of
        {ok, FieldType} ->
            {ok, [FieldType | Stack]};
        error ->
            resolve_via_registry(Name, Stack, Locals)
    end.

resolve_via_registry(Name, Stack, _Locals) ->
    %% 3. Look up in the type registry via TOS-type dispatch.
    case find_op_for_inference(Name, Stack) of
        {ok, #operation{sig_in = OpSigIn, sig_out = OpSigOut}} ->
            apply_stack_effect(OpSigIn, OpSigOut, Stack, Name);
        not_found ->
            %% 4. Literal detection: integer, float, boolean.
            case try_literal_type(Name) of
                {ok, Type} -> {ok, [Type | Stack]};
                not_found ->
                    %% 5. Is the name registered under SOME type — just not
                    %% matching against the current stack (sig mismatch or
                    %% stack too shallow)? If so, this is a real stack-effect
                    %% error, not a forward-reference. Reporting it as
                    %% stack_underflow / type_mismatch lets the caller raise
                    %% properly rather than masking with an Atom push.
                    case af_type:find_op_by_name(Name, 'Any') of
                        {ok, #operation{sig_in = OpSigIn, sig_out = OpSigOut}} ->
                            apply_stack_effect(OpSigIn, OpSigOut, Stack, Name);
                        not_found ->
                            case find_op_by_name_in_any_type(Name) of
                                {ok, #operation{sig_in = OpSigIn, sig_out = OpSigOut}} ->
                                    apply_stack_effect(OpSigIn, OpSigOut, Stack, Name);
                                not_found ->
                                    %% Truly unknown — likely a forward
                                    %% reference. Push Atom; the caller
                                    %% defers until the name resolves.
                                    {ok, ['Atom' | Stack]}
                            end
                    end
            end
    end.

%% Find an op by name across ALL registered types (used after TOS-type and
%% Any have both failed). Returns the first match, which is enough to
%% decide "is this a known name?" The caller cares about sig/stack-effect.
find_op_by_name_in_any_type(Name) ->
    Types = af_type:all_types(),
    find_op_by_name_in_types(Types, Name).

find_op_by_name_in_types([], _Name) -> not_found;
find_op_by_name_in_types([Type | Rest], Name) ->
    TypeName = Type#af_type.name,
    case af_type:find_op_by_name(Name, TypeName) of
        {ok, _Op} = Ok -> Ok;
        not_found -> find_op_by_name_in_types(Rest, Name)
    end.

%% Walk the type stack until we hit a 'ListBuilder' sentinel.
%% Returns {ok, StackBelowBuilder, CountAbove} | no_builder.
collect_to_list_builder([], _N) -> no_builder;
collect_to_list_builder(['ListBuilder' | Rest], N) -> {ok, Rest, N};
collect_to_list_builder([{'ListBuilder', _} | Rest], N) -> {ok, Rest, N};
collect_to_list_builder([_Item | Rest], N) ->
    collect_to_list_builder(Rest, N + 1).

%% Try to determine if a token name is a literal value.
try_literal_type("True")  -> {ok, 'Bool'};
try_literal_type("true")  -> {ok, 'Bool'};
try_literal_type("False") -> {ok, 'Bool'};
try_literal_type("false") -> {ok, 'Bool'};
try_literal_type(Name) ->
    case catch list_to_integer(Name) of
        N when is_integer(N) -> {ok, 'Int'};
        _ ->
            case catch list_to_float(Name) of
                F when is_float(F) -> {ok, 'Float'};
                _ -> not_found
            end
    end.

%% Find operation for type inference purposes.
%% Builds a mock stack from the type stack so af_type's sig matching works.
find_op_for_inference(Name, TypeStack) ->
    MockStack = types_to_mock_stack(TypeStack),
    case af_type:find_op_in_tos(Name, MockStack) of
        {ok, Op} -> {ok, Op};
        not_found ->
            case af_type:find_op_in_any(Name, MockStack) of
                {ok, Op} -> {ok, Op};
                not_found -> not_found
            end
    end.

%% Convert a type stack (list of atoms or {Type, Value} constraints) to
%% mock stack items that af_type:match_sig can match against.
types_to_mock_stack(TypeStack) ->
    lists:map(fun
        ({Type, Value}) -> {Type, Value};
        (Type) when is_atom(Type) -> {Type, undefined}
    end, TypeStack).

%% Apply a stack effect: consume sig_in types, produce sig_out types.
%% Named type variables (_name) bind to concrete types positionally.
%% Anonymous wildcard (_) and 'Any' match anything without tracking.
apply_stack_effect(SigIn, SigOut, Stack, Name) ->
    case consume_types(SigIn, Stack) of
        {ok, RemainingStack, Bindings} ->
            %% Substitute type variables in sig_out with concrete types
            ResolvedOut = [resolve_type(T, Bindings) || T <- SigOut],
            NewStack = ResolvedOut ++ RemainingStack,
            {ok, NewStack};
        {error, Reason} ->
            {error, {stack_underflow, Name, Reason}}
    end.

%% Consume types from the stack according to sig_in.
%% Returns {ok, RemainingStack, Bindings} where Bindings maps type variable
%% names to the concrete types that were matched.
%%
%% Type variables:
%%   'Any' or '_' — anonymous wildcard, matches anything, binding not tracked
%%   '_name'      — named type variable, binds to concrete type on first use,
%%                  subsequent uses must match the same type
consume_types(SigIn, Stack) ->
    consume_types(SigIn, Stack, #{}).

consume_types([], Stack, Bindings) -> {ok, Stack, Bindings};
consume_types([_SigType | _], [], _Bindings) -> {error, stack_empty};
consume_types([SigType | SigRest], [StackType | StackRest], Bindings) ->
    case classify_sig_type(SigType) of
        anonymous_wildcard ->
            %% 'Any' or '_' — matches anything, no binding tracked
            consume_types(SigRest, StackRest, Bindings);
        {named_variable, VarName} ->
            %% Named type variable — bind on first use, verify on subsequent
            case maps:find(VarName, Bindings) of
                {ok, BoundType} ->
                    %% Already bound — verify compatible
                    case type_compatible(BoundType, StackType) of
                        true ->
                            consume_types(SigRest, StackRest, Bindings);
                        false ->
                            {error, {expected, BoundType, got, StackType}}
                    end;
                error ->
                    %% First binding
                    NewBindings = Bindings#{VarName => StackType},
                    consume_types(SigRest, StackRest, NewBindings)
            end;
        concrete_type ->
            case type_compatible(SigType, StackType) of
                true -> consume_types(SigRest, StackRest, Bindings);
                false -> {error, {expected, SigType, got, StackType}}
            end
    end.

%% Classify a signature type entry.
classify_sig_type('Any') -> anonymous_wildcard;
classify_sig_type('_') -> anonymous_wildcard;
classify_sig_type(Type) when is_atom(Type) ->
    case is_type_variable(Type) of
        true -> {named_variable, Type};
        false -> concrete_type
    end;
classify_sig_type(_) -> concrete_type.  %% {Type, Value} constraints

%% Check if an atom is a named type variable: starts with _ followed by
%% at least one character (e.g., '_a', '_1', '_alpha').
%% Bare '_' is NOT a named variable — it's an anonymous wildcard.
is_type_variable(Atom) when is_atom(Atom) ->
    case atom_to_list(Atom) of
        [$_ | Rest] when Rest =/= [] -> true;
        _ -> false
    end;
is_type_variable(_) -> false.

%% Resolve type variables in output signatures using bindings from input matching.
resolve_type('Any', Bindings) ->
    %% Legacy: if there's exactly one binding, resolve 'Any' to it for
    %% backward compatibility with ops that still use plain 'Any' sigs.
    case maps:size(Bindings) of
        1 -> hd(maps:values(Bindings));
        _ -> 'Any'
    end;
resolve_type('_', _Bindings) -> 'Any';  %% anonymous wildcard stays generic
resolve_type(Type, Bindings) when is_atom(Type) ->
    case is_type_variable(Type) of
        true ->
            case maps:find(Type, Bindings) of
                {ok, ConcreteType} -> ConcreteType;
                error -> Type  %% unbound — keep as-is
            end;
        false -> Type
    end;
resolve_type(Type, _Bindings) -> Type.  %% {Type, Value} constraints pass through

%% Check if a stack type is compatible with a signature type.
type_compatible('Any', _StackType) -> true;
type_compatible('_', _StackType) -> true;
type_compatible(SigType, _StackType) when is_atom(SigType) ->
    case is_type_variable(SigType) of
        true -> true;  %% type variables match anything
        false -> type_compatible_concrete(SigType, _StackType)
    end;
type_compatible(SigType, StackType) ->
    type_compatible_concrete(SigType, StackType).

type_compatible_concrete(SigType, SigType) -> true;
type_compatible_concrete(_SigType, 'Any') -> true;
type_compatible_concrete({Type, _Value}, Type) -> true;
type_compatible_concrete(Type, {Type, _Value}) -> true;
type_compatible_concrete({Type, _V1}, {Type, _V2}) -> true;
type_compatible_concrete(_, _) -> false.

%% Check if the result stack matches the declared output signature.
match_output(ResultStack, SigOut) ->
    match_types(ResultStack, SigOut).

match_types([], []) -> true;
match_types([R | RRest], [S | SRest]) ->
    type_compatible(R, S) andalso match_types(RRest, SRest);
match_types(_, _) -> false.

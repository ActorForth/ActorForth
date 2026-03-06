-module(af_type_check).

-include("operation.hrl").
-include("af_type.hrl").

-export([check_word/4, infer_stack/2]).

%% Check a word definition's body against its declared signatures.
%% Returns ok | {error, Reason}.
%%
%% SigIn/SigOut: declared type signatures (TOS-first ordering).
%% Body: list of #operation{} with name fields (as produced by resolve_compile_token).
%%
%% Strategy: start with a type stack matching SigIn, apply each body token's
%% stack effect, then verify the result matches SigOut.
check_word(Name, SigIn, SigOut, Body) ->
    %% Build initial type stack from SigIn (TOS-first = list head is TOS)
    InitStack = SigIn,
    case infer_stack(Body, InitStack) of
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
infer_stack([], Stack) -> {ok, Stack};
infer_stack([#operation{name = Name} | Rest], Stack) ->
    case resolve_stack_effect(Name, Stack) of
        {ok, NewStack} ->
            infer_stack(Rest, NewStack);
        {error, _} = Err ->
            Err
    end.

%% Determine the stack effect of a named operation given the current type stack.
%% Returns {ok, NewStack} | {error, Reason}.
resolve_stack_effect(Name, Stack) ->
    %% Try to find the operation via the type system
    case find_op_for_inference(Name, Stack) of
        {ok, #operation{sig_in = OpSigIn, sig_out = OpSigOut}} ->
            apply_stack_effect(OpSigIn, OpSigOut, Stack, Name);
        not_found ->
            %% Unknown token — assume it pushes an Atom
            {ok, ['Atom' | Stack]}
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
%% When sig_in contains 'Any', build a substitution map so sig_out 'Any'
%% entries resolve to the concrete type from the stack.
apply_stack_effect(SigIn, SigOut, Stack, Name) ->
    case consume_types(SigIn, Stack) of
        {ok, RemainingStack, Bindings} ->
            %% Substitute Any in sig_out with concrete types from bindings
            ResolvedOut = [resolve_type(T, Bindings) || T <- SigOut],
            NewStack = ResolvedOut ++ RemainingStack,
            {ok, NewStack};
        {error, Reason} ->
            {error, {stack_underflow, Name, Reason}}
    end.

%% Consume types from the stack according to sig_in.
%% Returns {ok, RemainingStack, Bindings} where Bindings maps 'Any' positions
%% to the concrete types that were matched.
consume_types(SigIn, Stack) ->
    consume_types(SigIn, Stack, #{}).

consume_types([], Stack, Bindings) -> {ok, Stack, Bindings};
consume_types([_SigType | _], [], _Bindings) -> {error, stack_empty};
consume_types(['Any' | SigRest], [StackType | StackRest], Bindings) ->
    %% 'Any' matches anything; record the binding
    NewBindings = Bindings#{'Any' => StackType},
    consume_types(SigRest, StackRest, NewBindings);
consume_types([SigType | SigRest], [StackType | StackRest], Bindings) ->
    case type_compatible(SigType, StackType) of
        true -> consume_types(SigRest, StackRest, Bindings);
        false -> {error, {expected, SigType, got, StackType}}
    end.

%% Resolve 'Any' in output types using bindings from input matching.
resolve_type('Any', #{'Any' := ConcreteType}) -> ConcreteType;
resolve_type(Type, _Bindings) -> Type.

%% Check if a stack type is compatible with a signature type.
type_compatible('Any', _StackType) -> true;
type_compatible(SigType, SigType) -> true;
type_compatible(_SigType, 'Any') -> true;  %% Unknown type on stack matches anything
type_compatible({Type, _Value}, Type) -> true;  %% Value constraint matches base type
type_compatible(Type, {Type, _Value}) -> true;  %% Base type matches value constraint
type_compatible(_, _) -> false.

%% Check if the result stack matches the declared output signature.
match_output(ResultStack, SigOut) ->
    match_types(ResultStack, SigOut).

match_types([], []) -> true;
match_types([R | RRest], [S | SRest]) ->
    type_compatible(R, S) andalso match_types(RRest, SRest);
match_types(_, _) -> false.

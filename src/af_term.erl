-module(af_term).

-include("af_type.hrl").

-export([to_stack_item/1, from_stack_item/1]).
-export([to_stack_items/1, from_stack_items/1]).

%% Convert Erlang term -> ActorForth stack item
-spec to_stack_item(term()) -> {atom(), term()}.
to_stack_item(true) -> {'Bool', true};
to_stack_item(false) -> {'Bool', false};
to_stack_item(N) when is_integer(N) -> {'Int', N};
to_stack_item(F) when is_float(F) -> {'Float', F};
to_stack_item(B) when is_binary(B) -> {'String', B};
to_stack_item(A) when is_atom(A) -> {'Atom', atom_to_list(A)};
to_stack_item(L) when is_list(L) ->
    {'List', [to_stack_item(E) || E <- L]};
to_stack_item(M) when is_map(M) ->
    {'Map', maps:from_list(
        [{to_stack_item(K), to_stack_item(V)} || {K, V} <- maps:to_list(M)]
    )};
to_stack_item(T) when is_tuple(T) ->
    {'Tuple', T};
to_stack_item(P) when is_pid(P) ->
    {'Actor', #{pid => P, type_name => undefined, vocab => #{}}};
to_stack_item(R) when is_reference(R) ->
    %% Refs flow as a first-class 'Ref' tag so they're never converted
    %% through `list_to_atom`. That would leak into the atom table
    %% (never GC'd) on every correlation round-trip and eventually
    %% crash the VM with atom-table exhaustion.
    {'Ref', R};
to_stack_item(Other) ->
    error({unsupported_term, Other}).

%% Convert ActorForth stack item -> Erlang term
-spec from_stack_item({atom(), term()}) -> term().
from_stack_item({'Int', N}) -> N;
from_stack_item({'Float', F}) -> F;
from_stack_item({'Bool', B}) -> B;
from_stack_item({'String', B}) -> B;
from_stack_item({'Atom', S}) when is_list(S)   -> list_to_atom(S);
from_stack_item({'Atom', A}) when is_atom(A)    -> A;
from_stack_item({'Atom', B}) when is_binary(B)  -> binary_to_atom(B, utf8);
from_stack_item({'Ref', R}) -> R;
from_stack_item({'Tuple', T}) -> T;
from_stack_item({'List', Items}) ->
    [from_stack_item(I) || I <- Items];
from_stack_item({'Map', M}) ->
    maps:from_list(
        [{from_stack_item(K), from_stack_item(V)} || {K, V} <- maps:to_list(M)]
    );
from_stack_item({'Actor', #{pid := Pid}}) -> Pid;
from_stack_item(Tuple) when is_tuple(Tuple), tuple_size(Tuple) >= 2 ->
    %% Product type instance: {TypeName, V1, V2, ..., Vn}.
    %% Values are raw (untagged) and field names come from the registry.
    TypeName = element(1, Tuple),
    case is_atom(TypeName) of
        false -> Tuple;
        true ->
            case catch af_type:get_type(TypeName) of
                {ok, #af_type{fields = Fields}} when Fields =/= [] ->
                    FieldPairs = lists:zip(
                        [F || {F, _T} <- Fields],
                        tl(tuple_to_list(Tuple))
                    ),
                    Converted = maps:from_list(FieldPairs),
                    Converted#{type => TypeName};
                _ -> Tuple
            end
    end.

%% Batch conversion
-spec to_stack_items([term()]) -> [{atom(), term()}].
to_stack_items(Terms) ->
    [to_stack_item(T) || T <- Terms].

-spec from_stack_items([{atom(), term()}]) -> [term()].
from_stack_items(Items) ->
    [from_stack_item(I) || I <- Items].

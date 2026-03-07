-module(prop_type_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("operation.hrl").
-include("af_type.hrl").

%% ============================================================
%% Generators
%% ============================================================

%% Generate a type name atom
type_name() ->
    oneof(['Int', 'Bool', 'String', 'Float', 'Atom', 'List', 'Map']).

%% Generate a typed stack item
stack_item() ->
    ?LET(Type, type_name(),
        {Type, gen_value(Type)}).

gen_value('Int') -> integer();
gen_value('Bool') -> oneof([true, false]);
gen_value('String') -> binary();
gen_value('Float') -> float();
gen_value('Atom') -> oneof(["foo", "bar", "baz"]);
gen_value('List') -> list(integer());
gen_value('Map') -> map(atom(), integer()).

%% Generate a non-empty stack of typed items
stack() ->
    non_empty(list(stack_item())).

%% ============================================================
%% Setup / Teardown
%% ============================================================

setup() ->
    af_type:reset().

%% ============================================================
%% Properties
%% ============================================================

%% Property 1: Empty sig matches any stack
empty_sig_matches_any_test() ->
    setup(),
    Prop = ?FORALL(Stack, stack(),
        af_type:match_sig([], Stack) =:= true),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Also: empty sig matches empty stack
empty_sig_empty_stack_test() ->
    setup(),
    ?assert(af_type:match_sig([], []) =:= true).

%% Property 2: A sig of all 'Any' atoms matches any stack of sufficient length
any_matches_everything_test() ->
    setup(),
    Prop = ?FORALL(Stack, stack(),
        begin
            Len = length(Stack),
            Sig = lists:duplicate(Len, 'Any'),
            af_type:match_sig(Sig, Stack) =:= true
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Partial 'Any' sig matches if stack is long enough
any_partial_match_test() ->
    setup(),
    Prop = ?FORALL(Stack, stack(),
        begin
            Len = length(Stack),
            case Len > 0 of
                true ->
                    SigLen = max(1, Len div 2),
                    Sig = lists:duplicate(SigLen, 'Any'),
                    af_type:match_sig(Sig, Stack) =:= true;
                false ->
                    true
            end
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 3: Exact type match - [{Type, Value}] on stack matches sig [Type]
exact_type_match_test() ->
    setup(),
    Prop = ?FORALL(Item, stack_item(),
        begin
            {Type, _Val} = Item,
            af_type:match_sig([Type], [Item]) =:= true
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Property 4: Value constraint match
value_constraint_match_test() ->
    setup(),
    Prop = ?FORALL(Item, stack_item(),
        begin
            {Type, Val} = Item,
            %% Exact {Type, Value} in sig matches same on stack
            af_type:match_sig([{Type, Val}], [Item]) =:= true
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

value_constraint_mismatch_test() ->
    setup(),
    %% Different values should not match
    ?assertNot(af_type:match_sig([{'Int', 42}], [{'Int', 99}])),
    ?assertNot(af_type:match_sig([{'Bool', true}], [{'Bool', false}])),
    ?assertNot(af_type:match_sig([{'Int', 1}], [{'Int', 2}])).

%% Property 5: Sig longer than stack never matches
length_mismatch_test() ->
    setup(),
    Prop = ?FORALL(Stack, stack(),
        begin
            Len = length(Stack),
            %% Make sig longer than stack
            Sig = lists:duplicate(Len + 1, 'Any'),
            af_type:match_sig(Sig, Stack) =:= false
        end),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Non-empty sig on empty stack never matches
nonempty_sig_empty_stack_test() ->
    setup(),
    Prop = ?FORALL(Type, type_name(),
        af_type:match_sig([Type], []) =:= false),
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

%% Type mismatch: wrong type name on stack
type_mismatch_test() ->
    setup(),
    ?assertNot(af_type:match_sig(['Int'], [{'Bool', true}])),
    ?assertNot(af_type:match_sig(['Bool'], [{'Int', 42}])),
    ?assertNot(af_type:match_sig(['String'], [{'Int', 1}])).

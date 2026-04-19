-module(af_type_list).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

%% List type: wraps native Erlang lists.
%% Representation: {List, [StackItem, ...]}
%%
%% nil       — ( -> List )              push empty list
%% cons      — ( List Any -> List )     prepend item to list
%% length    — ( List -> Int )          count items (destructive; use dup first)
%% head      — ( List -> Any )          first item (error on empty)
%% tail      — ( List -> List )         rest of list (error on empty)
%% append    — ( List List -> List )    concatenate two lists
%% reverse   — ( List -> List )         reverse a list
%% nth       — ( List Int -> Any )      get nth element (0-based)
%% last      — ( List -> Any )          get last element
%% take      — ( List Int -> List )     take first N elements
%% drop      — ( List Int -> List )     drop first N elements
%% empty?    — ( List -> Bool )         check if list is empty
%% contains? — ( List Any -> Bool )     check if item is in list
%% flatten   — ( List -> List )         flatten nested lists one level
%% zip       — ( List List -> List )    combine two lists into list of Tuple pairs
%% map       — ( Atom List -> List )   apply word to each element
%% filter    — ( Atom List -> List )   keep elements where word returns Bool true
%% reduce    — ( Atom Any List -> Any ) fold with word (Item Acc -> NewAcc)
%% each      — ( Atom List -> )        apply word to each element for side effects

init() ->
    af_type:register_type(#af_type{name = 'List'}),
    af_type:register_type(#af_type{name = 'ListBuilder'}),

    %% nil: push empty list
    af_type:add_op('Any', #operation{
        name = "nil", sig_in = [], sig_out = ['List'],
        impl = fun op_nil/1
    }),

    %% cons: item on TOS, list below -> new list on TOS
    af_type:add_op('Any', #operation{
        name = "cons", sig_in = ['Any', 'List'], sig_out = ['List'],
        impl = fun op_cons/1
    }),

    %% [ and ] — bracket list literals.
    %% `[` pushes a ListBuilder sentinel. `]` walks the data stack
    %% back to the nearest sentinel, collecting everything above it
    %% into a source-ordered List. Both registered on Any with no
    %% sig_in constraints: `]` has to fire regardless of TOS because
    %% arbitrary values between the brackets become the list contents.
    %% `[ 1 2 3 ]` -> {List, [1, 2, 3]} (first item pushed = first in list).
    af_type:add_op('Any', #operation{
        name = "[", sig_in = [], sig_out = ['ListBuilder'],
        impl = fun op_list_start/1
    }),
    af_type:add_op('Any', #operation{
        name = "]", sig_in = [], sig_out = ['List'],
        impl = fun op_list_end/1
    }),

    %% length: list -> int
    af_type:add_op('List', #operation{
        name = "length", sig_in = ['List'], sig_out = ['Int'],
        impl = fun op_length/1
    }),

    %% head: list -> first item
    af_type:add_op('List', #operation{
        name = "head", sig_in = ['List'], sig_out = ['Any'],
        impl = fun op_head/1
    }),

    %% tail: list -> rest of list
    af_type:add_op('List', #operation{
        name = "tail", sig_in = ['List'], sig_out = ['List'],
        impl = fun op_tail/1
    }),

    %% append: two lists -> concatenated list
    af_type:add_op('List', #operation{
        name = "append", sig_in = ['List', 'List'], sig_out = ['List'],
        impl = fun op_append/1
    }),

    %% reverse: list -> reversed list
    af_type:add_op('List', #operation{
        name = "reverse", sig_in = ['List'], sig_out = ['List'],
        impl = fun op_reverse/1
    }),

    %% nth: Int on TOS, List below -> element at index
    af_type:add_op('Int', #operation{
        name = "nth", sig_in = ['Int', 'List'], sig_out = ['Any'],
        impl = fun op_nth/1
    }),

    %% last: list -> last element
    af_type:add_op('List', #operation{
        name = "last", sig_in = ['List'], sig_out = ['Any'],
        impl = fun op_last/1
    }),

    %% take: Int on TOS, List below -> list of first N items
    af_type:add_op('Int', #operation{
        name = "take", sig_in = ['Int', 'List'], sig_out = ['List'],
        impl = fun op_take/1
    }),

    %% drop: Int on TOS, List below -> list without first N items
    af_type:add_op('Int', #operation{
        name = "drop", sig_in = ['Int', 'List'], sig_out = ['List'],
        impl = fun op_drop/1
    }),

    %% empty?: list -> bool
    af_type:add_op('List', #operation{
        name = "empty?", sig_in = ['List'], sig_out = ['Bool'],
        impl = fun op_empty/1
    }),

    %% contains?: Any on TOS, List below -> bool
    af_type:add_op('Any', #operation{
        name = "contains?", sig_in = ['Any', 'List'], sig_out = ['Bool'],
        impl = fun op_contains/1
    }),

    %% flatten: list of lists -> flat list
    af_type:add_op('List', #operation{
        name = "flatten", sig_in = ['List'], sig_out = ['List'],
        impl = fun op_flatten/1
    }),

    %% zip: two lists -> list of Tuple pairs
    af_type:add_op('List', #operation{
        name = "zip", sig_in = ['List', 'List'], sig_out = ['List'],
        impl = fun op_zip/1
    }),

    %% map: apply word to each element -> new list
    af_type:add_op('Atom', #operation{
        name = "map", sig_in = ['Atom', 'List'], sig_out = ['List'],
        impl = fun op_map/1
    }),

    %% filter: keep elements where word returns true
    af_type:add_op('Atom', #operation{
        name = "filter", sig_in = ['Atom', 'List'], sig_out = ['List'],
        impl = fun op_filter/1
    }),

    %% reduce: fold list with word, starting from initial value
    af_type:add_op('Atom', #operation{
        name = "reduce", sig_in = ['Atom', 'Any', 'List'], sig_out = ['Any'],
        impl = fun op_reduce/1
    }),

    %% each: apply word to each element for side effects
    af_type:add_op('Atom', #operation{
        name = "each", sig_in = ['Atom', 'List'], sig_out = [],
        impl = fun op_each/1
    }),

    ok.

%%% Operations

op_nil(Cont) ->
    Cont#continuation{
        data_stack = [{'List', []} | Cont#continuation.data_stack]
    }.

op_cons(Cont) ->
    [Item, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', [Item | Items]} | Rest]}.

%% `[` — push a ListBuilder sentinel. Two-tuple so match_sig's
%% `element(1, Tuple)` type check works (it requires tuple_size >= 2).
op_list_start(Cont) ->
    Cont#continuation{
        data_stack = [{'ListBuilder', start} | Cont#continuation.data_stack]
    }.

%% `]` — walk back to the nearest ListBuilder sentinel, collect
%% everything above it in source (first-pushed-first) order, and
%% push the resulting List. Nested `[ ... ]` work automatically
%% because each `[` pushes its own marker and `]` only collects
%% back to the nearest one.
op_list_end(Cont) ->
    {Items, Below} = collect_to_marker(Cont#continuation.data_stack, []),
    Cont#continuation{data_stack = [{'List', Items} | Below]}.

collect_to_marker([{'ListBuilder', start} | Rest], Acc) ->
    %% Acc was built by prepending each popped item, which reverses
    %% the pop order. Pop order = TOS-first = last-pushed-first.
    %% So Acc is now in first-pushed-first = source order.
    {Acc, Rest};
collect_to_marker([Item | Rest], Acc) ->
    collect_to_marker(Rest, [Item | Acc]);
collect_to_marker([], Acc) ->
    %% Unterminated `[` — treat the rest as the collected items.
    %% Arguably should raise, but preserving whatever was there is
    %% safer for interactive REPL use.
    {Acc, []}.

op_length(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', length(Items)} | Rest]}.

op_head(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    case Items of
        [H | _] -> Cont#continuation{data_stack = [H | Rest]};
        [] -> af_error:raise(empty_list, "head on empty list", Cont)
    end.

op_tail(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    case Items of
        [_ | T] -> Cont#continuation{data_stack = [{'List', T} | Rest]};
        [] -> af_error:raise(empty_list, "tail on empty list", Cont)
    end.

op_append(Cont) ->
    [{'List', B}, {'List', A} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', A ++ B} | Rest]}.

op_reverse(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', lists:reverse(Items)} | Rest]}.

op_nth(Cont) ->
    [{'Int', N}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    case N >= 0 andalso N < length(Items) of
        true ->
            Elem = lists:nth(N + 1, Items),
            Cont#continuation{data_stack = [Elem | Rest]};
        false ->
            af_error:raise(index_out_of_range, "nth index out of range", Cont)
    end.

op_last(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    case Items of
        [] -> af_error:raise(empty_list, "last on empty list", Cont);
        _ -> Cont#continuation{data_stack = [lists:last(Items) | Rest]}
    end.

op_take(Cont) ->
    [{'Int', N}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Taken = lists:sublist(Items, N),
    Cont#continuation{data_stack = [{'List', Taken} | Rest]}.

op_drop(Cont) ->
    [{'Int', N}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Dropped = lists:nthtail(min(N, length(Items)), Items),
    Cont#continuation{data_stack = [{'List', Dropped} | Rest]}.

op_empty(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', Items =:= []} | Rest]}.

op_contains(Cont) ->
    [Item, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Bool', lists:member(Item, Items)} | Rest]}.

op_flatten(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    Flattened = lists:flatmap(fun({'List', SubItems}) -> SubItems;
                                 (Other) -> [Other]
                              end, Items),
    Cont#continuation{data_stack = [{'List', Flattened} | Rest]}.

op_zip(Cont) ->
    [{'List', B}, {'List', A} | Rest] = Cont#continuation.data_stack,
    Zipped = lists:zipwith(fun(X, Y) -> {'Tuple', {X, Y}} end, A, B),
    Cont#continuation{data_stack = [{'List', Zipped} | Rest]}.

%% Higher-order list operations — dispatch word by name via interpreter

apply_word(WordName, StackItems, Cont) ->
    TempCont = Cont#continuation{data_stack = StackItems},
    Token = #token{value = WordName, line = 0, column = 0, file = "list-op"},
    af_interpreter:interpret_token(Token, TempCont).

op_map(Cont) ->
    [{'Atom', WordName}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Mapped = lists:map(fun(Item) ->
        Result = apply_word(WordName, [Item], Cont),
        hd(Result#continuation.data_stack)
    end, Items),
    Cont#continuation{data_stack = [{'List', Mapped} | Rest]}.

op_filter(Cont) ->
    [{'Atom', WordName}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Filtered = lists:filter(fun(Item) ->
        Result = apply_word(WordName, [Item], Cont),
        case hd(Result#continuation.data_stack) of
            {'Bool', true} -> true;
            _ -> false
        end
    end, Items),
    Cont#continuation{data_stack = [{'List', Filtered} | Rest]}.

op_reduce(Cont) ->
    [{'Atom', WordName}, Acc, {'List', Items} | Rest] = Cont#continuation.data_stack,
    FinalAcc = lists:foldl(fun(Item, CurAcc) ->
        Result = apply_word(WordName, [Item, CurAcc], Cont),
        hd(Result#continuation.data_stack)
    end, Acc, Items),
    Cont#continuation{data_stack = [FinalAcc | Rest]}.

op_each(Cont) ->
    [{'Atom', WordName}, {'List', Items} | Rest] = Cont#continuation.data_stack,
    lists:foreach(fun(Item) ->
        apply_word(WordName, [Item], Cont)
    end, Items),
    Cont#continuation{data_stack = Rest}.

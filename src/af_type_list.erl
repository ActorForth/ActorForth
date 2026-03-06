-module(af_type_list).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% List type: wraps native Erlang lists.
%% Representation: {List, [StackItem, ...]}
%%
%% nil    — ( -> List )         push empty list
%% cons   — ( List Any -> List )  prepend item to list
%% length — ( List -> Int )     count items (destructive; use dup first)
%% head   — ( List -> Any )     first item (error on empty)
%% tail   — ( List -> List )    rest of list (error on empty)

init() ->
    af_type:register_type(#af_type{name = 'List'}),

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

    ok.

%%% Operations

op_nil(Cont) ->
    Cont#continuation{
        data_stack = [{'List', []} | Cont#continuation.data_stack]
    }.

op_cons(Cont) ->
    [Item, {'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', [Item | Items]} | Rest]}.

op_length(Cont) ->
    [{'List', Items} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', length(Items)} | Rest]}.

op_head(Cont) ->
    [{'List', [H | _]} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [H | Rest]}.

op_tail(Cont) ->
    [{'List', [_ | T]} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'List', T} | Rest]}.

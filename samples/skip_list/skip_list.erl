%% skip_list.erl — Erlang reference implementation of skip_list.a4.
%%
%% Functional skip list: state is a record carrying a dict of {K, V} plus
%% four ordered level lists. Pure functions, no gen_server.
%%
%%   erlc skip_list.erl && erl -noshell -s skip_list run -s init stop

-module(skip_list).
-export([new/0, insert/3, delete/2, has/2, get/2, size/1, keys/1, run/0]).

-define(MAX_LEVEL, 4).

-record(sl, {data = #{}   :: map(),
             levels = [[], [], [], []] :: list()}).

new() -> #sl{}.

insert(Key, Value, #sl{data = D, levels = Ls} = S) ->
    Ls1 = case maps:is_key(Key, D) of
              true  -> Ls;  % updating existing: keep levels as-is
              false ->
                  L = random_level(),
                  promote(Key, 0, L, Ls)
          end,
    S#sl{data = maps:put(Key, Value, D), levels = Ls1}.

delete(Key, #sl{data = D, levels = Ls} = S) ->
    case maps:is_key(Key, D) of
        false -> S;
        true  ->
            Ls1 = [lists:delete(Key, L) || L <- Ls],
            S#sl{data = maps:remove(Key, D), levels = Ls1}
    end.

has(Key, #sl{data = D}) -> maps:is_key(Key, D).

get(Key, #sl{data = D}) -> maps:get(Key, D).

size(#sl{data = D}) -> maps:size(D).

keys(#sl{levels = [L0 | _]}) -> L0.

%%% Internal

random_level() -> random_level(0).
random_level(N) when N >= ?MAX_LEVEL - 1 -> N;
random_level(N) ->
    case rand:uniform(2) of
        1 -> random_level(N + 1);
        _ -> N
    end.

promote(_Key, Lvl, MaxLvl, Ls) when Lvl > MaxLvl -> Ls;
promote(Key, Lvl, MaxLvl, Ls) ->
    L = lists:nth(Lvl + 1, Ls),
    LNew = sorted_insert(Key, L),
    Ls1 = replace_nth(Lvl + 1, LNew, Ls),
    promote(Key, Lvl + 1, MaxLvl, Ls1).

sorted_insert(X, []) -> [X];
sorted_insert(X, [H | T]) when X =< H -> [X, H | T];
sorted_insert(X, [H | T]) -> [H | sorted_insert(X, T)].

replace_nth(N, V, L) ->
    lists:sublist(L, N - 1) ++ [V] ++ lists:nthtail(N, L).

%%% Test driver

run() ->
    rand:seed(exsplus, {42, 42, 42}),

    % sorted-insert
    Lst = lists:foldl(fun sorted_insert/2, [], [3, 1, 5, 2]),
    1 = hd(Lst),
    4 = length(Lst),
    io:format("sorted-insert: ok~n"),

    % empty
    S0 = new(),
    0 = ?MODULE:size(S0),
    io:format("sl-new: ok~n"),

    % single insert
    S1 = insert(10, 100, S0),
    true = has(10, S1),
    100 = ?MODULE:get(10, S1),
    1 = ?MODULE:size(S1),
    io:format("single insert: ok~n"),

    % multiple inserts
    S2 = lists:foldl(fun({K, V}, Acc) -> insert(K, V, Acc) end,
                     new(), [{5,50},{3,30},{8,80},{1,10}]),
    4 = ?MODULE:size(S2),
    10 = ?MODULE:get(1, S2),
    30 = ?MODULE:get(3, S2),
    50 = ?MODULE:get(5, S2),
    80 = ?MODULE:get(8, S2),
    1 = hd(keys(S2)),
    io:format("multi insert: ok~n"),

    % delete
    S3 = delete(3, S2),
    3 = ?MODULE:size(S3),
    false = has(3, S3),
    io:format("delete: ok~n"),

    % delete non-existent
    S4 = delete(99, S3),
    3 = ?MODULE:size(S4),

    % update
    S5 = insert(5, 999, S4),
    999 = ?MODULE:get(5, S5),
    io:format("update: ok~n"),

    % larger
    S6 = lists:foldl(fun({K, V}, A) -> insert(K, V, A) end,
                     new(),
                     [{20,200},{10,100},{30,300},{15,150},
                      {25,250},{5,50},{35,350}]),
    7 = ?MODULE:size(S6),
    50 = ?MODULE:get(5, S6),
    350 = ?MODULE:get(35, S6),
    io:format("Skip list with 7 entries:~n"),
    {Ls} = {S6#sl.levels},
    lists:foldl(fun(L, I) ->
        io:format("  Level ~p: ~p keys~n", [I, length(L)]),
        I + 1
    end, 0, Ls),
    io:format("large dataset: ok~n"),
    ok.

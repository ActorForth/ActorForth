%% auction.erl — Erlang reference implementation of the a4 auction demo.
%%
%% Idiomatic Erlang: gen_server per auction, record-based state,
%% pattern-matched handle_call/3. Compile and run with:
%%
%%   erlc auction.erl && erl -noshell -s auction run -s init stop

-module(auction).
-behaviour(gen_server).

-export([start_link/3, bid/3, close/1, leader/1, leading_bid/1, is_open/1,
         run/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    item           :: binary(),
    current_bid    :: integer(),
    highest_bidder :: binary(),
    status = open :: open | closed
}).

%%% API

start_link(Item, StartBid, StartBidder) ->
    gen_server:start_link(?MODULE, [Item, StartBid, StartBidder], []).

bid(Pid, Amount, Bidder) ->
    gen_server:call(Pid, {bid, Amount, Bidder}).

close(Pid) ->
    gen_server:call(Pid, close).

leader(Pid) ->
    gen_server:call(Pid, leader).

leading_bid(Pid) ->
    gen_server:call(Pid, leading_bid).

is_open(Pid) ->
    gen_server:call(Pid, is_open).

%%% gen_server callbacks

init([Item, StartBid, StartBidder]) ->
    {ok, #state{item = Item,
                current_bid = StartBid,
                highest_bidder = StartBidder}}.

handle_call({bid, _Amount, _Bidder}, _From, #state{status = closed} = S) ->
    {reply, false, S};
handle_call({bid, Amount, _Bidder}, _From,
            #state{current_bid = Current} = S) when Amount =< Current ->
    {reply, false, S};
handle_call({bid, Amount, Bidder}, _From, S) ->
    {reply, true, S#state{current_bid = Amount, highest_bidder = Bidder}};
handle_call(close, _From, S) ->
    {reply, ok, S#state{status = closed}};
handle_call(leader, _From, S) ->
    {reply, S#state.highest_bidder, S};
handle_call(leading_bid, _From, S) ->
    {reply, S#state.current_bid, S};
handle_call(is_open, _From, S) ->
    {reply, S#state.status =:= open, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

%%% Test driver

run() ->
    {ok, A} = start_link(<<"laptop">>, 100, <<>>),

    true  = bid(A, 150, <<"alice">>),
    <<"alice">> = leader(A),
    150   = leading_bid(A),

    false = bid(A, 120, <<"bob">>),      % too low
    <<"alice">> = leader(A),
    150   = leading_bid(A),

    true  = bid(A, 200, <<"bob">>),
    <<"bob">> = leader(A),
    200   = leading_bid(A),

    close(A),
    false = is_open(A),

    false = bid(A, 500, <<"carol">>),    % closed
    <<"bob">> = leader(A),
    200   = leading_bid(A),

    io:format("auction.erl: all assertions passed~n"),
    ok.

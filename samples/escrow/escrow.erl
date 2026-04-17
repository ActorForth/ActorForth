%% escrow.erl — Erlang reference implementation of the a4 escrow contract.
%%
%%   erlc escrow.erl && erl -noshell -s escrow run -s init stop

-module(escrow).
-behaviour(gen_server).

-export([start_link/4, deposit/3, verify/2, release/1, dispute/2,
         state/1, is_released/1, is_refunded/1, run/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    buyer       :: binary(),
    seller      :: binary(),
    verifier    :: binary(),
    amount      :: integer(),
    status = pending :: pending | funded | verified | disputed | released | refunded
}).

%%% API

start_link(Buyer, Seller, Verifier, Amount) ->
    gen_server:start_link(?MODULE, [Buyer, Seller, Verifier, Amount], []).

deposit(Pid, Amount, Caller) -> gen_server:call(Pid, {deposit, Amount, Caller}).
verify(Pid, Caller)          -> gen_server:call(Pid, {verify, Caller}).
release(Pid)                  -> gen_server:call(Pid, release).
dispute(Pid, Caller)          -> gen_server:call(Pid, {dispute, Caller}).
state(Pid)                    -> gen_server:call(Pid, state).
is_released(Pid)              -> gen_server:call(Pid, is_released).
is_refunded(Pid)              -> gen_server:call(Pid, is_refunded).

%%% gen_server callbacks

init([Buyer, Seller, Verifier, Amount]) ->
    {ok, #state{buyer = Buyer, seller = Seller,
                verifier = Verifier, amount = Amount}}.

handle_call({deposit, Amount, Caller}, _From,
            #state{status = pending, amount = Amount, buyer = Caller} = S) ->
    {reply, ok, S#state{status = funded}};
handle_call({deposit, _, _}, _From, S) ->
    {reply, ok, S};

handle_call({verify, Caller}, _From,
            #state{status = funded, verifier = Caller} = S) ->
    {reply, ok, S#state{status = verified}};
handle_call({verify, _}, _From, S) ->
    {reply, ok, S};

handle_call(release, _From, #state{status = verified} = S) ->
    {reply, ok, S#state{status = released}};
handle_call(release, _From, S) ->
    {reply, ok, S};

handle_call({dispute, Caller}, _From,
            #state{status = funded, buyer = Buyer, seller = Seller} = S)
  when Caller =:= Buyer; Caller =:= Seller ->
    {reply, ok, S#state{status = refunded}};
handle_call({dispute, _}, _From, S) ->
    {reply, ok, S};

handle_call(state, _From, S)       -> {reply, S#state.status, S};
handle_call(is_released, _From, S) -> {reply, S#state.status =:= released, S};
handle_call(is_refunded, _From, S) -> {reply, S#state.status =:= refunded, S}.

handle_cast(_Msg, S) -> {noreply, S}.

%%% Test driver

run() ->
    % Happy path
    {ok, E1} = start_link(<<"alice">>, <<"bob">>, <<"oracle">>, 1000),
    pending = state(E1),
    deposit(E1, 1000, <<"alice">>),
    funded = state(E1),
    verify(E1, <<"oracle">>),
    verified = state(E1),
    release(E1),
    true = is_released(E1),
    io:format("escrow.erl happy path: passed~n"),

    % State guards
    {ok, E2} = start_link(<<"alice">>, <<"bob">>, <<"oracle">>, 500),
    verify(E2, <<"oracle">>),      % too early
    pending = state(E2),
    release(E2),                    % too early
    pending = state(E2),
    deposit(E2, 500, <<"alice">>),
    release(E2),                    % before verify
    funded = state(E2),
    verify(E2, <<"oracle">>),
    deposit(E2, 999, <<"alice">>),  % too late
    verified = state(E2),
    release(E2),
    release(E2),                    % no-op
    true = is_released(E2),
    io:format("escrow.erl state guards: passed~n"),

    % Dispute path
    {ok, E3} = start_link(<<"alice">>, <<"bob">>, <<"oracle">>, 750),
    deposit(E3, 750, <<"alice">>),
    dispute(E3, <<"alice">>),
    true = is_refunded(E3),
    release(E3),                    % can't release after refund
    true = is_refunded(E3),
    io:format("escrow.erl dispute path: passed~n"),
    ok.

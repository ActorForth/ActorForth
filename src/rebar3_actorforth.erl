-module(rebar3_actorforth).

-export([init/1]).

%% rebar3 plugin entry point.
%% Register the ActorForth compiler provider.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_actorforth_compile:init(State),
    {ok, State1}.

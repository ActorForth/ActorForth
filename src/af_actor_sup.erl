-module(af_actor_sup).
-behaviour(supervisor).

-export([start_link/0, start_actor/2, stop_actor/1]).
-export([init/1]).

%% Start the actor supervisor (should be started once at application init)
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Start a supervised actor process.
%% TypeName: the type of the actor's state
%% StateInstance: the initial state {TypeName, FieldMap}
%% Returns {ok, Pid}
start_actor(TypeName, StateInstance) ->
    supervisor:start_child(?MODULE, [TypeName, StateInstance]).

%% Stop a supervised actor
stop_actor(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% Supervisor callback
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => af_actor_worker,
        start => {af_actor_worker, start_link, []},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [af_actor_worker]
    },
    {ok, {SupFlags, [ChildSpec]}}.

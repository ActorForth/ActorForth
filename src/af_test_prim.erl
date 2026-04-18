-module(af_test_prim).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Test-DSL VM primitives. Each is a thin FFI over continuation fields:
%%
%%   trace-on         ( -- )         enable traced dispatch
%%   trace-off        ( -- )         disable traced dispatch, clear fields
%%   capture-data     ( -- List )    snapshot data stack as List
%%   capture-return   ( -- List )    snapshot return stack as List
%%   capture-exec     ( -- List )    snapshot exec stack as List of ExecEvents
%%   get-trace        ( -- List )    alias for capture-exec
%%   get-coverage     ( -- Map )     accumulated coverage (Phase 1: empty)
%%   depth-stats-map  ( -- Map )     #depth_stats{} as Map of Ints
%%   dict-snapshot    ( -- DictRef ) snapshot current dictionary
%%   dict-restore     ( DictRef -- ) revert dictionary from snapshot
%%
%% All primitives register on the Any type dictionary — they are stateless
%% on the data stack (except dict-restore, which consumes a DictRef).
%%
%% ETS-mutation audit (Phase 1 exit criterion):
%%   - None of the primitives above call af_type:add_op, add_type, or any
%%     ETS-writing API. Reads only.
%%   - dict-snapshot calls af_type:snapshot/0 (ETS read, no write) when
%%     operating on a legacy continuation with no local dictionary.
%%   - dict-restore mutates only the continuation's dictionary field and
%%     clears the dispatch_cache — no ETS touch.
%% Conclusion: test primitives are ETS-write-free.
%%
%% When Phase 2 adds DSL words like `group`, `test`, `setup` that define
%% new vocabulary inside a test, those ops MUST route through
%% af_type:dict_add_op/3 (continuation-local) to keep parallel tests
%% isolated. af_type:add_op/2 (ETS) would leak across test actors.

init() ->
    af_type:add_op('Any', #operation{
        name = "trace-on", sig_in = [], sig_out = [],
        impl = fun op_trace_on/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "trace-off", sig_in = [], sig_out = [],
        impl = fun op_trace_off/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "capture-data", sig_in = [], sig_out = ['List'],
        impl = fun op_capture_data/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "capture-return", sig_in = [], sig_out = ['List'],
        impl = fun op_capture_return/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "capture-exec", sig_in = [], sig_out = ['List'],
        impl = fun op_capture_exec/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "get-trace", sig_in = [], sig_out = ['List'],
        impl = fun op_capture_exec/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "get-coverage", sig_in = [], sig_out = ['Map'],
        impl = fun op_get_coverage/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "depth-stats-map", sig_in = [], sig_out = ['Map'],
        impl = fun op_depth_stats_map/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "dict-snapshot", sig_in = [], sig_out = ['DictRef'],
        impl = fun op_dict_snapshot/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "dict-restore", sig_in = ['DictRef'], sig_out = [],
        impl = fun op_dict_restore/1, source = af_test_prim
    }),
    ok.

%%% Implementations

op_trace_on(Cont) ->
    Cont#continuation{
        tracing = true,
        exec_stack = [],
        depth_stats = #depth_stats{}
    }.

op_trace_off(Cont) ->
    Cont#continuation{
        tracing = false,
        exec_stack = [],
        depth_stats = undefined
    }.

op_capture_data(#continuation{data_stack = Stack} = Cont) ->
    ListVal = {'List', Stack},
    Cont#continuation{data_stack = [ListVal | Stack]}.

op_capture_return(#continuation{data_stack = DS, return_stack = RS} = Cont) ->
    ListVal = {'List', RS},
    Cont#continuation{data_stack = [ListVal | DS]}.

op_capture_exec(#continuation{data_stack = DS, exec_stack = ES} = Cont) ->
    %% Wrap raw events as {'ExecEvent', RawTuple} so list items are
    %% well-formed {Type, Value} stack items.
    Events = [{'ExecEvent', E} || E <- ES],
    ListVal = {'List', Events},
    Cont#continuation{data_stack = [ListVal | DS]}.

op_get_coverage(#continuation{data_stack = DS} = Cont) ->
    %% Phase 1: empty map. Coverage accumulation is a Phase 1.5 task.
    MapVal = {'Map', #{}},
    Cont#continuation{data_stack = [MapVal | DS]}.

op_depth_stats_map(#continuation{data_stack = DS, depth_stats = Stats0} = Cont) ->
    Stats = case Stats0 of
        undefined -> #depth_stats{};
        S -> S
    end,
    Map = #{
        <<"data_max">>   => {'Int', Stats#depth_stats.data_max},
        <<"data_sum">>   => {'Int', Stats#depth_stats.data_sum},
        <<"return_max">> => {'Int', Stats#depth_stats.return_max},
        <<"count">>      => {'Int', Stats#depth_stats.count}
    },
    MapVal = {'Map', Map},
    Cont#continuation{data_stack = [MapVal | DS]}.

op_dict_snapshot(#continuation{data_stack = DS, dictionary = Dict0} = Cont) ->
    Dict = case Dict0 of
        undefined -> af_type:snapshot();  %% legacy path: snapshot ETS into a map
        D -> D
    end,
    Ref = {'DictRef', Dict},
    Cont#continuation{data_stack = [Ref | DS]}.

op_dict_restore(#continuation{data_stack = [{'DictRef', Dict} | Rest]} = Cont) ->
    %% Restoring the dictionary invalidates any cached dispatch entries
    %% built against the old dict, so clear the cache too.
    Cont#continuation{
        dictionary = Dict,
        data_stack = Rest,
        dispatch_cache = #{}
    };
op_dict_restore(_Cont) ->
    erlang:error({dict_restore, no_dictref_on_tos}).

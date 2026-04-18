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

    %% Exec-stack manipulation (same primitives the plan mentions under
    %% "Execution-stack words (also usable outside tests)").
    af_type:add_op('Any', #operation{
        name = "exec-depth", sig_in = [], sig_out = ['Int'],
        impl = fun op_exec_depth/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "exec-peek", sig_in = [], sig_out = ['ExecEvent'],
        impl = fun op_exec_peek/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "exec-pop", sig_in = [], sig_out = ['ExecEvent'],
        impl = fun op_exec_pop/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "exec-drop", sig_in = [], sig_out = [],
        impl = fun op_exec_drop/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "exec-clear", sig_in = [], sig_out = [],
        impl = fun op_exec_clear/1, source = af_test_prim
    }),
    af_type:add_op('Any', #operation{
        name = "exec-reverse", sig_in = [], sig_out = [],
        impl = fun op_exec_reverse/1, source = af_test_prim
    }),

    %% Exec event accessors — crack open an {exec, Token, Stack, Kind} tuple.
    af_type:add_op('ExecEvent', #operation{
        name = "event-token", sig_in = ['ExecEvent'], sig_out = ['ExecEvent', 'Map'],
        impl = fun op_event_token/1, source = af_test_prim
    }),
    af_type:add_op('ExecEvent', #operation{
        name = "event-stack", sig_in = ['ExecEvent'], sig_out = ['ExecEvent', 'List'],
        impl = fun op_event_stack/1, source = af_test_prim
    }),
    af_type:add_op('ExecEvent', #operation{
        name = "event-kind", sig_in = ['ExecEvent'], sig_out = ['ExecEvent', 'Atom'],
        impl = fun op_event_kind/1, source = af_test_prim
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

op_get_coverage(#continuation{data_stack = DS, coverage = Cov} = Cont) ->
    %% Convert the coverage map into an a4 Map: FileBin => {'Int', visits}
    %% where visits is the total count summed across all positions.
    M = maps:map(fun(_File, Positions) ->
        Total = lists:sum(maps:values(Positions)),
        {'Int', Total}
    end, Cov),
    Cont#continuation{data_stack = [{'Map', M} | DS]}.

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

%%% Exec-stack manipulation

op_exec_depth(#continuation{data_stack = DS, exec_stack = ES} = Cont) ->
    Cont#continuation{data_stack = [{'Int', length(ES)} | DS]}.

op_exec_peek(#continuation{data_stack = DS, exec_stack = [E | _]} = Cont) ->
    Cont#continuation{data_stack = [{'ExecEvent', E} | DS]};
op_exec_peek(_Cont) ->
    erlang:error({exec_peek, empty}).

op_exec_pop(#continuation{data_stack = DS, exec_stack = [E | Rest]} = Cont) ->
    Cont#continuation{data_stack = [{'ExecEvent', E} | DS], exec_stack = Rest};
op_exec_pop(_Cont) ->
    erlang:error({exec_pop, empty}).

op_exec_drop(#continuation{exec_stack = [_ | Rest]} = Cont) ->
    Cont#continuation{exec_stack = Rest};
op_exec_drop(Cont) ->
    Cont.  %% drop on empty is a no-op

op_exec_clear(Cont) ->
    Cont#continuation{exec_stack = []}.

op_exec_reverse(#continuation{exec_stack = ES} = Cont) ->
    Cont#continuation{exec_stack = lists:reverse(ES)}.

%%% Exec-event accessors
%%%
%%% These leave the ExecEvent on the stack (non-destructive) and push the
%%% requested component on top. The "-" between e.g. event and token is
%%% deliberate: Forth-style word naming keeps the verb/noun ordering.

op_event_token(#continuation{data_stack = [{'ExecEvent', {exec, Token, _, _}} = Ev | Rest]} = Cont) ->
    Map = token_to_map(Token),
    Cont#continuation{data_stack = [{'Map', Map}, Ev | Rest]}.

op_event_stack(#continuation{data_stack = [{'ExecEvent', {exec, _, Stack, _}} = Ev | Rest]} = Cont) ->
    Cont#continuation{data_stack = [{'List', Stack}, Ev | Rest]}.

op_event_kind(#continuation{data_stack = [{'ExecEvent', {exec, _, _, Kind}} = Ev | Rest]} = Cont) ->
    KindAtom = case Kind of
        atom                 -> "atom";
        {tos, _}             -> "tos";
        {any, _}             -> "any";
        handler              -> "handler";
        {literal, _}         -> "literal";
        {cache_hit, _}       -> "cache_hit"
    end,
    Cont#continuation{data_stack = [{'Atom', KindAtom}, Ev | Rest]}.

token_to_map(Token) ->
    #{
        <<"value">>  => {'String', list_to_binary(Token#token.value)},
        <<"file">>   => {'String', list_to_binary(Token#token.file)},
        <<"line">>   => {'Int',    Token#token.line},
        <<"column">> => {'Int',    Token#token.column},
        <<"quoted">> => {'Bool',   Token#token.quoted}
    }.

-record(depth_stats, {
    data_max   = 0 :: non_neg_integer(),
    data_sum   = 0 :: non_neg_integer(),
    return_max = 0 :: non_neg_integer(),
    count      = 0 :: non_neg_integer()
}).

-record(continuation, {
    data_stack    = [] :: [af_type:stack_item()],
    return_stack  = [] :: list(),
    current_token = undefined :: #token{} | undefined,
    debug         = false :: boolean(),
    word_trace    = [] :: list(),
    dictionary    = undefined :: map() | undefined,  %% TypeName => #af_type{}
    dispatch_cache = #{} :: map(),  %% {Value, TosType} => {op, Impl} | {literal, TV}
    %% --- test DSL fields: zero production cost when tracing = false ---
    tracing       = false :: boolean(),               %% master test-mode flag
    exec_stack    = []    :: list(),                  %% dispatch events, newest-first
    depth_stats   = undefined :: undefined | #depth_stats{},
    %% Coverage: map of FileBinary => #{{Line, Col} => 1} for positions
    %% visited under traced dispatch. Updated per token when tracing is on.
    coverage      = #{}   :: map(),
    %% --- test-DSL registration state ---
    %% Active group scopes live as {'GroupScope', #{...}} sentinels on
    %% data_stack itself, so multi-clause dispatch on `.` can close them.
    test_registry = []    :: list()                   %% registered test specs
}).

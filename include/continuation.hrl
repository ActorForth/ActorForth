-record(depth_stats, {
    data_max   = 0 :: non_neg_integer(),
    data_sum   = 0 :: non_neg_integer(),
    count      = 0 :: non_neg_integer()
}).

-record(continuation, {
    data_stack    = [] :: [af_type:stack_item()],
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
    test_registry = []    :: list(),                  %% registered test specs
    %% --- Auto-field bindings ---
    %% Stack of frames; each frame is a #{BindingName => StackItem} map.
    %% Populated by words whose sig_in contains a product type — the
    %% compiler synthesizes a wrapper that pushes a frame at entry with
    %% the product's fields bound to their names (plus positional .x
    %% / ..x forms). The interpreter consults the top frame before
    %% dispatching each body token; a matching name pushes the bound
    %% value instead of dispatching as an op.
    locals        = []    :: list()
}).

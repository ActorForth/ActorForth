-record(continuation, {
    data_stack    = [] :: [af_type:stack_item()],
    return_stack  = [] :: list(),
    current_token = undefined :: #token{} | undefined,
    debug         = false :: boolean(),
    word_trace    = [] :: list(),
    dictionary    = undefined :: map() | undefined,  %% TypeName => #af_type{}
    dispatch_cache = #{} :: map()  %% {Value, TosType} => {op, Impl} | {literal, TV}
}).

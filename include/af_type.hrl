-record(af_type, {
    name    = undefined :: atom() | undefined,
    ops     = #{}       :: map(),        %% #{string() => [#operation{}]}
    handler = undefined :: function() | undefined
}).

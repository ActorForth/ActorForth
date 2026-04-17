-record(af_type, {
    name    = undefined :: atom() | undefined,
    ops     = #{}       :: map(),        %% #{string() => [#operation{}]}
    handler = undefined :: function() | undefined,
    %% Product types only: ordered list of {FieldName :: atom, FieldType :: atom}
    %% pairs. Instance storage is {TypeName, V1, V2, ...} where field i is at
    %% tuple position i+1. Empty list for non-product types.
    fields  = []        :: [{atom(), atom()}]
}).

-record(af_type, {
    name    = undefined :: atom() | undefined,
    ops     = #{}       :: map(),        %% #{string() => [#operation{}]}
    %% Handler slot. Two shapes are accepted:
    %%   - `fun((Value, Cont) -> Cont)` — the classic Erlang hook.
    %%   - `atom()` — names an a4 word. When a token dispatches through
    %%      such a handler, af_interpreter pushes `{'String', BinValue}`
    %%      onto the data_stack and dispatches the named a4 word; the
    %%      word is responsible for any stack bookkeeping the handler
    %%      would otherwise do. Added for #180 so the DSL's `system ...
    %%      end` token interceptor can live in a4 source.
    handler = undefined :: function() | atom() | undefined,
    %% Product types only: ordered list of {FieldName :: atom, FieldType :: atom}
    %% pairs. Instance storage is {TypeName, V1, V2, ...} where field i is at
    %% tuple position i+1. Empty list for non-product types.
    fields  = []        :: [{atom(), atom()}]
}).

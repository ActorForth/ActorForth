-record(operation, {
    name     = ""        :: string(),
    sig_in   = []        :: list(),      %% [type_constraint()]
    sig_out  = []        :: list(),      %% [type_constraint()]
    impl     = undefined :: function() | undefined,
    source   = undefined :: term()       %% #token{} | undefined
}).

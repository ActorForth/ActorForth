-record(token, {
    value  = "" :: string(),
    column = 0  :: non_neg_integer(),
    line   = 0  :: non_neg_integer(),
    file   = "" :: string(),
    quoted = false :: boolean()
}).

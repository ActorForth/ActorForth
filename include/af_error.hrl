-record(af_error, {
    type       :: atom(),
    message    :: string(),
    token      :: #token{} | undefined,
    stack      :: list(),
    word_trace :: list()
}).

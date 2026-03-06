-module(af_error).

-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

-export([raise/3, raise/4, format/1, format_value/1]).

%% Raise a structured error
-spec raise(atom(), string(), #continuation{}) -> no_return().
raise(Type, Message, Cont) ->
    error(#af_error{
        type = Type,
        message = Message,
        token = Cont#continuation.current_token,
        stack = Cont#continuation.data_stack,
        word_trace = Cont#continuation.word_trace
    }).

-spec raise(atom(), string(), #continuation{}, list()) -> no_return().
raise(Type, Message, Cont, Extra) ->
    error(#af_error{
        type = Type,
        message = Message ++ format_extra(Extra),
        token = Cont#continuation.current_token,
        stack = Cont#continuation.data_stack,
        word_trace = Cont#continuation.word_trace
    }).

%% Format an af_error for display
-spec format(#af_error{}) -> string().
format(#af_error{type = Type, message = Msg, token = Token, stack = Stack, word_trace = Trace}) ->
    Location = format_location(Token),
    TraceStr = format_trace(Trace),
    StackStr = format_stack(Stack),
    lists:flatten(io_lib:format(
        "Error: ~p~s~n  ~s~s~s",
        [Type, Location, Msg, TraceStr, StackStr]
    )).

%% Format a single stack item for display
format_value({'Int', N}) -> integer_to_list(N);
format_value({'Bool', true}) -> "True";
format_value({'Bool', false}) -> "False";
format_value({'String', B}) -> "\"" ++ binary_to_list(B) ++ "\"";
format_value({'Atom', S}) -> S;
format_value({'List', Items}) ->
    Inner = string:join([format_value(I) || I <- Items], ", "),
    "[" ++ Inner ++ "]";
format_value({'Map', _}) -> "<Map>";
format_value({'Actor', _}) -> "<Actor>";
format_value({Type, _}) -> atom_to_list(Type) ++ "(...)";
format_value(Other) -> lists:flatten(io_lib:format("~p", [Other])).

%%% Internal

format_location(undefined) -> "";
format_location(#token{file = File, line = Line, column = Col}) ->
    lists:flatten(io_lib:format(" at ~s:~p:~p", [File, Line, Col])).

format_trace([]) -> "";
format_trace(Trace) ->
    Lines = lists:map(fun({WordName, Token}) ->
        Loc = format_location(Token),
        lists:flatten(io_lib:format("~n  in word '~s'~s", [WordName, Loc]))
    end, Trace),
    lists:flatten(Lines).

format_stack([]) -> "\n  Stack: (empty)";
format_stack(Stack) ->
    Items = [format_typed(I) || I <- lists:sublist(Stack, 5)],
    Suffix = case length(Stack) > 5 of true -> " ..."; false -> "" end,
    lists:flatten(io_lib:format("~n  Stack(~p): ~s~s", [
        length(Stack),
        string:join(Items, " "),
        Suffix
    ])).

format_typed({Type, Val}) ->
    lists:flatten(io_lib:format("~s:~s", [format_value({Type, Val}), Type]));
format_typed(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).

format_extra([]) -> "";
format_extra(Extra) ->
    lists:flatten([io_lib:format("; ~p", [E]) || E <- Extra]).

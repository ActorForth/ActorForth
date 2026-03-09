%% af_r0_parser.erl -- Self-hosted A4 parser using Ring 0 data model
%%
%% Each function operates on tagged values ({Type, Value}) and corresponds
%% to a Ring 0 word. Tokens are maps with value/line/col/quoted keys.
%% This replaces af_parser.erl in the Ring 2 pipeline.
%%
%% Usage:
%%   Tokens = af_r0_parser:parse(<<"source code">>, <<"filename">>).
%%   %% Returns [{token, #{value => <<"...">>, line => 1, col => 1, ...}}, ...]

-module(af_r0_parser).

-export([parse/2]).

%%% === Public API ===

%% Parse A4 source string into list of token maps.
%% Source and Filename are binaries.
parse(Source, Filename) when is_binary(Source), is_binary(Filename) ->
    State = #{
        src => Source,
        file => Filename,
        pos => 0,
        line => 1,
        col => 1,
        len => byte_size(Source)
    },
    Tokens = tokenize(State, []),
    lists:reverse(Tokens).

%%% === Tokenizer ===

tokenize(#{pos := Pos, len := Len}, Acc) when Pos >= Len ->
    Acc;
tokenize(State, Acc) ->
    State1 = skip_ws(State),
    case at_end(State1) of
        true -> Acc;
        false ->
            Ch = peek(State1),
            case Ch of
                $# ->
                    State2 = skip_comment(State1),
                    tokenize(State2, Acc);
                $" ->
                    {Token, State2} = read_string(advance(State1)),
                    tokenize(State2, [Token | Acc]);
                $: -> tokenize_self_delim(State1, <<":">>, Acc);
                $; -> tokenize_self_delim(State1, <<";">>, Acc);
                $. ->
                    case is_float_dot(State1, Acc) of
                        true ->
                            %% Advance past dot, read remaining digits
                            State2 = advance(State1),
                            {Digits, State3} = accumulate(State2, <<>>),
                            %% Merge: prevtoken + "." + digits
                            [PrevTok | AccRest] = Acc,
                            PrevVal = maps:get(value, PrevTok),
                            Merged = PrevTok#{value => <<PrevVal/binary, ".", Digits/binary>>},
                            tokenize(State3, [Merged | AccRest]);
                        false ->
                            tokenize_self_delim(State1, <<".">>, Acc)
                    end;
                _ ->
                    {Token, State2} = read_token(State1),
                    tokenize(State2, [Token | Acc])
            end
    end.

tokenize_self_delim(State, Value, Acc) ->
    #{line := Line, col := Col, file := File} = State,
    Token = make_token(Value, Line, Col, File, false),
    tokenize(advance(State), [Token | Acc]).

%%% === Token Reading ===

read_token(#{line := Line, col := Col, file := File} = State) ->
    {Value, State1} = accumulate(State, <<>>),
    Token = make_token(Value, Line, Col, File, false),
    {Token, State1}.

accumulate(State, Acc) ->
    case at_end(State) of
        true -> {Acc, State};
        false ->
            Ch = peek(State),
            case is_delimiter(Ch) of
                true -> {Acc, State};
                false ->
                    accumulate(advance(State), <<Acc/binary, Ch>>)
            end
    end.

is_delimiter(Ch) ->
    Ch =:= $\s orelse Ch =:= $\t orelse Ch =:= $\n orelse Ch =:= $\r
    orelse Ch =:= $: orelse Ch =:= $; orelse Ch =:= $.
    orelse Ch =:= $# orelse Ch =:= $".

read_string(State) ->
    read_string(State, <<>>).

read_string(State, Acc) ->
    case at_end(State) of
        true ->
            %% Unterminated string
            #{line := Line, col := Col, file := File} = State,
            Token = make_token(Acc, Line, Col, File, true),
            {Token, State};
        false ->
            Ch = peek(State),
            case Ch of
                $" ->
                    %% Get the position of the opening quote for the token
                    %% The token line/col should be where the string content starts
                    #{file := File} = State,
                    %% Calculate original start position
                    StartCol = maps:get(col, State) - byte_size(Acc),
                    StartLine = maps:get(line, State),
                    Token = make_token(Acc, StartLine, StartCol, File, true),
                    {Token, advance(State)};
                $\n ->
                    read_string(advance_newline(State), <<Acc/binary, $\n>>);
                _ ->
                    read_string(advance(State), <<Acc/binary, Ch>>)
            end
    end.

%%% === Whitespace & Comments ===

skip_ws(State) ->
    case at_end(State) of
        true -> State;
        false ->
            Ch = peek(State),
            case Ch of
                $\s -> skip_ws(advance(State));
                $\t -> skip_ws(advance_tab(State));
                $\n -> skip_ws(advance_newline(State));
                $\r -> skip_ws(advance(State));
                _ -> State
            end
    end.

skip_comment(State) ->
    case at_end(State) of
        true -> State;
        false ->
            case peek(State) of
                $\n -> advance_newline(State);
                _ -> skip_comment(advance(State))
            end
    end.

%%% === Float Dot Detection ===

%% Check if a dot at current position is part of a float literal.
%% Requires: previous token is all digits, and next char is a digit.
is_float_dot(State, [PrevTok | _]) ->
    %% Check next char after dot is a digit
    NextPos = maps:get(pos, State) + 1,
    case NextPos < maps:get(len, State) of
        false -> false;
        true ->
            NextCh = binary:at(maps:get(src, State), NextPos),
            case is_digit(NextCh) of
                false -> false;
                true ->
                    %% Check previous token is all digits (and adjacent)
                    PrevVal = maps:get(value, PrevTok),
                    PrevQuoted = maps:get(quoted, PrevTok),
                    case PrevQuoted of
                        true -> false;
                        false ->
                            %% Check adjacency: prev token ends right before dot
                            PrevCol = maps:get(col, PrevTok),
                            PrevLen = byte_size(PrevVal),
                            DotCol = maps:get(col, State),
                            case PrevCol + PrevLen =:= DotCol of
                                false -> false;
                                true -> all_digits(PrevVal)
                            end
                    end
            end
    end;
is_float_dot(_, []) -> false.

all_digits(<<>>) -> false;
all_digits(<<$-, Rest/binary>>) -> all_digits_strict(Rest);
all_digits(Bin) -> all_digits_strict(Bin).

all_digits_strict(<<>>) -> false;
all_digits_strict(Bin) ->
    lists:all(fun(B) -> is_digit(B) end, binary_to_list(Bin)).

is_digit(Ch) -> Ch >= $0 andalso Ch =< $9.

%%% === State Navigation ===

at_end(#{pos := Pos, len := Len}) -> Pos >= Len.

peek(#{src := Src, pos := Pos}) -> binary:at(Src, Pos).

advance(#{pos := Pos, col := Col} = State) ->
    State#{pos => Pos + 1, col => Col + 1}.

advance_newline(#{pos := Pos, line := Line} = State) ->
    State#{pos => Pos + 1, line => Line + 1, col => 1}.

advance_tab(#{pos := Pos, col := Col} = State) ->
    State#{pos => Pos + 1, col => Col + 4}.

%%% === Token Construction ===

make_token(Value, Line, Col, File, Quoted) ->
    #{
        value => Value,
        line => Line,
        col => Col,
        file => File,
        quoted => Quoted
    }.

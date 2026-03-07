-module(af_type_string).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

init() ->
    af_type:register_type(#af_type{name = 'String'}),

    %% Constructor: Atom -> String
    af_type:add_op('Any', #operation{
        name = "string",
        sig_in = ['Atom'],
        sig_out = ['String'],
        impl = fun op_string/1
    }),

    %% Pass-through: String -> String
    af_type:add_op('Any', #operation{
        name = "string",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_string_passthrough/1
    }),

    %% concat: String String -> String
    af_type:add_op('String', #operation{
        name = "concat",
        sig_in = ['String', 'String'],
        sig_out = ['String'],
        impl = fun op_concat/1
    }),

    %% length: String -> Int
    af_type:add_op('String', #operation{
        name = "length",
        sig_in = ['String'],
        sig_out = ['Int'],
        impl = fun op_length/1
    }),

    %% to-atom: String -> Atom
    af_type:add_op('String', #operation{
        name = "to-atom",
        sig_in = ['String'],
        sig_out = ['Atom'],
        impl = fun op_to_atom/1
    }),

    %% to-int: String -> Int
    af_type:add_op('String', #operation{
        name = "to-int",
        sig_in = ['String'],
        sig_out = ['Int'],
        impl = fun op_to_int/1
    }),

    %% to-string: Int -> String
    af_type:add_op('Int', #operation{
        name = "to-string",
        sig_in = ['Int'],
        sig_out = ['String'],
        impl = fun op_int_to_string/1
    }),

    %% split: String String -> List  (split TOS=delimiter, below=target)
    af_type:add_op('String', #operation{
        name = "split",
        sig_in = ['String', 'String'],
        sig_out = ['List'],
        impl = fun op_split/1
    }),

    %% contains: String String -> Bool  (TOS=needle, below=haystack)
    af_type:add_op('String', #operation{
        name = "contains",
        sig_in = ['String', 'String'],
        sig_out = ['Bool'],
        impl = fun op_contains/1
    }),

    %% starts-with: String String -> Bool  (TOS=prefix, below=target)
    af_type:add_op('String', #operation{
        name = "starts-with",
        sig_in = ['String', 'String'],
        sig_out = ['Bool'],
        impl = fun op_starts_with/1
    }),

    %% ends-with: String String -> Bool  (TOS=suffix, below=target)
    af_type:add_op('String', #operation{
        name = "ends-with",
        sig_in = ['String', 'String'],
        sig_out = ['Bool'],
        impl = fun op_ends_with/1
    }),

    %% trim: String -> String
    af_type:add_op('String', #operation{
        name = "trim",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_trim/1
    }),

    %% to-upper: String -> String
    af_type:add_op('String', #operation{
        name = "to-upper",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_to_upper/1
    }),

    %% to-lower: String -> String
    af_type:add_op('String', #operation{
        name = "to-lower",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_to_lower/1
    }),

    %% reverse: String -> String
    af_type:add_op('String', #operation{
        name = "reverse",
        sig_in = ['String'],
        sig_out = ['String'],
        impl = fun op_reverse/1
    }),

    %% replace: String String String -> String
    %% TOS=target, below=replacement, below that=pattern
    af_type:add_op('String', #operation{
        name = "replace",
        sig_in = ['String', 'String', 'String'],
        sig_out = ['String'],
        impl = fun op_replace/1
    }),

    %% substring: Int Int String -> String
    %% TOS=target string, below=length, below that=start index (0-based)
    af_type:add_op('String', #operation{
        name = "substring",
        sig_in = ['String', 'Int', 'Int'],
        sig_out = ['String'],
        impl = fun op_substring/1
    }),

    ok.

%%% Operations

op_string(Cont) ->
    [{'Atom', Value} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', list_to_binary(Value)} | Rest]}.

op_string_passthrough(Cont) -> Cont.

op_concat(Cont) ->
    [{'String', A}, {'String', B} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', <<B/binary, A/binary>>} | Rest]}.

op_length(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', byte_size(S)} | Rest]}.

op_to_atom(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Atom', binary_to_list(S)} | Rest]}.

op_to_int(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'Int', binary_to_integer(S)} | Rest]}.

op_int_to_string(Cont) ->
    [{'Int', N} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [{'String', integer_to_binary(N)} | Rest]}.

op_split(Cont) ->
    [{'String', Delim}, {'String', Target} | Rest] = Cont#continuation.data_stack,
    Parts = binary:split(Target, Delim, [global]),
    Items = [{'String', P} || P <- Parts],
    Cont#continuation{data_stack = [{'List', Items} | Rest]}.

op_contains(Cont) ->
    [{'String', Needle}, {'String', Haystack} | Rest] = Cont#continuation.data_stack,
    Result = case Needle of
        <<>> -> true;
        _ ->
            case binary:match(Haystack, Needle) of
                nomatch -> false;
                _ -> true
            end
    end,
    Cont#continuation{data_stack = [{'Bool', Result} | Rest]}.

op_starts_with(Cont) ->
    [{'String', Prefix}, {'String', Target} | Rest] = Cont#continuation.data_stack,
    PLen = byte_size(Prefix),
    Result = case Target of
        <<Prefix:PLen/binary, _/binary>> -> true;
        _ -> false
    end,
    Cont#continuation{data_stack = [{'Bool', Result} | Rest]}.

op_ends_with(Cont) ->
    [{'String', Suffix}, {'String', Target} | Rest] = Cont#continuation.data_stack,
    SLen = byte_size(Suffix),
    TLen = byte_size(Target),
    Result = case TLen >= SLen of
        true ->
            Skip = TLen - SLen,
            case Target of
                <<_:Skip/binary, Suffix:SLen/binary>> -> true;
                _ -> false
            end;
        false -> false
    end,
    Cont#continuation{data_stack = [{'Bool', Result} | Rest]}.

op_trim(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Trimmed = string:trim(binary_to_list(S)),
    Cont#continuation{data_stack = [{'String', list_to_binary(Trimmed)} | Rest]}.

op_to_upper(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Upper = string:uppercase(binary_to_list(S)),
    Cont#continuation{data_stack = [{'String', unicode:characters_to_binary(Upper)} | Rest]}.

op_to_lower(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Lower = string:lowercase(binary_to_list(S)),
    Cont#continuation{data_stack = [{'String', unicode:characters_to_binary(Lower)} | Rest]}.

op_reverse(Cont) ->
    [{'String', S} | Rest] = Cont#continuation.data_stack,
    Reversed = list_to_binary(lists:reverse(binary_to_list(S))),
    Cont#continuation{data_stack = [{'String', Reversed} | Rest]}.

op_replace(Cont) ->
    [{'String', Target}, {'String', Replacement}, {'String', Pattern} | Rest] = Cont#continuation.data_stack,
    Result = binary:replace(Target, Pattern, Replacement, [global]),
    Cont#continuation{data_stack = [{'String', Result} | Rest]}.

op_substring(Cont) ->
    [{'String', Target}, {'Int', Len}, {'Int', Start} | Rest] = Cont#continuation.data_stack,
    Sub = binary:part(Target, Start, Len),
    Cont#continuation{data_stack = [{'String', Sub} | Rest]}.

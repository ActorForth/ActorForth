%% af_ring0.erl -- Ring 0: Minimal primitive instruction set for ActorForth
%%
%% ~35 primitives that define A4's core semantics.
%% Every target environment (BEAM, FPGA, etc.) must implement these.
%% This is the BEAM target implementation.
%%
%% Stack values are tagged: {Type :: atom(), Value :: term()}
%% The type registry is a map of maps, fully inspectable from A4.
%% Types, pattern matching, and actors are first-class primitives.

-module(af_ring0).

-export([
    new/0, run/2, exec/2,
    get_ds/1, get_rs/1, get_types/1, get_words/1,
    get_output/1, set_input/2, set_ds/2, set_words/2, set_types/2
]).

-record(r0, {
    ds     = [] :: list(),       %% data stack (tagged values)
    rs     = [] :: list(),       %% return stack
    types  = #{} :: map(),       %% type registry: #{Name => #{ops, handler}}
    words  = #{} :: map(),       %% word table: #{Name => [Instruction]}
    output = <<>> :: binary(),   %% output buffer
    input  = <<>> :: binary()    %% input buffer
}).

%%% === State Management ===

new() -> #r0{}.

get_ds(#r0{ds = DS}) -> DS.
get_rs(#r0{rs = RS}) -> RS.
get_types(#r0{types = T}) -> T.
get_words(#r0{words = W}) -> W.
get_output(#r0{output = O}) -> O.
set_input(Bin, S) -> S#r0{input = Bin}.
set_ds(DS, S) -> S#r0{ds = DS}.
set_words(W, S) -> S#r0{words = W}.
set_types(T, S) -> S#r0{types = T}.

%%% === Program Execution ===

run(Program, State) ->
    Arr = list_to_tuple(Program),
    run_loop(Arr, 1, State).

run_loop(Arr, IP, State) when IP > tuple_size(Arr) -> State;
run_loop(Arr, IP, State) ->
    Inst = element(IP, Arr),
    case exec(Inst, State) of
        {branch, Target, S1} -> run_loop(Arr, Target, S1);
        {stop, S1} -> S1;
        S1 -> run_loop(Arr, IP + 1, S1)
    end.

%%% === Stack Primitives (5) ===

exec(dup, #r0{ds = [A | Rest]} = S) ->
    S#r0{ds = [A, A | Rest]};

exec(drop, #r0{ds = [_ | Rest]} = S) ->
    S#r0{ds = Rest};

exec(swap, #r0{ds = [A, B | Rest]} = S) ->
    S#r0{ds = [B, A | Rest]};

exec(to_r, #r0{ds = [A | DRest], rs = RS} = S) ->
    S#r0{ds = DRest, rs = [A | RS]};

exec(from_r, #r0{ds = DS, rs = [A | RRest]} = S) ->
    S#r0{ds = [A | DS], rs = RRest};

%%% === Data Primitives (4) ===

exec({lit, Value}, #r0{ds = DS} = S) ->
    S#r0{ds = [Value | DS]};

exec({tuple_new, N}, #r0{ds = DS} = S) ->
    {Items, Rest} = lists:split(N, DS),
    S#r0{ds = [list_to_tuple(lists:reverse(Items)) | Rest]};

exec({tuple_get, I}, #r0{ds = [Tuple | Rest]} = S) when is_tuple(Tuple) ->
    S#r0{ds = [element(I, Tuple) | Rest]};

exec({tuple_put, I}, #r0{ds = [Val, Tuple | Rest]} = S) when is_tuple(Tuple) ->
    S#r0{ds = [setelement(I, Tuple, Val) | Rest]};

%%% === List Primitives (4) ===

exec(nil, #r0{ds = DS} = S) ->
    S#r0{ds = [{'List', []} | DS]};

exec(cons, #r0{ds = [Elem, {'List', Tail} | Rest]} = S) ->
    S#r0{ds = [{'List', [Elem | Tail]} | Rest]};

exec(head, #r0{ds = [{'List', [H | _]} | Rest]} = S) ->
    S#r0{ds = [H | Rest]};

exec(tail, #r0{ds = [{'List', [_ | T]} | Rest]} = S) ->
    S#r0{ds = [{'List', T} | Rest]};

%%% === Arithmetic Primitives (5) ===

exec(add, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    S#r0{ds = [tag_number(B + A) | Rest]};

exec(sub, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    S#r0{ds = [tag_number(B - A) | Rest]};

exec(mul, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    S#r0{ds = [tag_number(B * A) | Rest]};

exec(divop, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    case is_float(A) orelse is_float(B) of
        true  -> S#r0{ds = [{'Float', B / A} | Rest]};
        false -> S#r0{ds = [{'Int', B div A} | Rest]}
    end;

exec(modop, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    S#r0{ds = [{'Int', B rem A} | Rest]};

%%% === Comparison + Logic (3) ===

exec(eq, #r0{ds = [A, B | Rest]} = S) ->
    S#r0{ds = [{'Bool', B =:= A} | Rest]};

exec(lt, #r0{ds = [{_, A}, {_, B} | Rest]} = S) ->
    S#r0{ds = [{'Bool', B < A} | Rest]};

exec(not_op, #r0{ds = [{'Bool', V} | Rest]} = S) ->
    S#r0{ds = [{'Bool', not V} | Rest]};

%%% === Control Flow (4) ===

exec({call, WordName}, #r0{words = Words} = S) ->
    Body = maps:get(WordName, Words),
    run(Body, S);

exec({branch_if, Target}, #r0{ds = [{'Bool', true} | Rest]} = S) ->
    {branch, Target, S#r0{ds = Rest}};
exec({branch_if, _Target}, #r0{ds = [{'Bool', false} | Rest]} = S) ->
    S#r0{ds = Rest};

exec({branch, Target}, S) ->
    {branch, Target, S};

exec(return, S) ->
    {stop, S};

%%% === Type System Primitives (5) ===
%%% Types are first-class. The registry is inspectable data.

%% type_new: pop {'Atom', Name}, create empty type
exec(type_new, #r0{ds = [{'Atom', Name} | Rest], types = Types} = S) ->
    NewType = #{ops => #{}, handler => undefined},
    S#r0{ds = Rest, types = Types#{Name => NewType}};

%% type_add_op: pop body, sig_out, sig_in, op_name, type_name
exec(type_add_op, #r0{ds = [Body, {'List', SigOut}, {'List', SigIn},
                             {'String', OpName}, {'Atom', TypeName} | Rest],
                      types = Types} = S) ->
    Type = maps:get(TypeName, Types, #{ops => #{}, handler => undefined}),
    Ops = maps:get(ops, Type),
    OpDef = #{sig_in => SigIn, sig_out => SigOut, body => Body},
    ExistingDefs = maps:get(OpName, Ops, []),
    NewOps = Ops#{OpName => ExistingDefs ++ [OpDef]},
    NewType = Type#{ops => NewOps},
    S#r0{ds = Rest, types = Types#{TypeName => NewType}};

%% type_get_op: pop op_name, type_name; push op list or not_found
exec(type_get_op, #r0{ds = [{'String', OpName}, {'Atom', TypeName} | Rest],
                      types = Types} = S) ->
    Result = case maps:get(TypeName, Types, undefined) of
        undefined -> {'Atom', not_found};
        Type ->
            Ops = maps:get(ops, Type),
            case maps:get(OpName, Ops, undefined) of
                undefined -> {'Atom', not_found};
                OpList -> {'List', OpList}
            end
    end,
    S#r0{ds = [Result | Rest]};

%% type_get: pop type_name, push full type map (inspectable)
exec(type_get, #r0{ds = [{'Atom', TypeName} | Rest], types = Types} = S) ->
    Result = case maps:get(TypeName, Types, undefined) of
        undefined -> {'Atom', not_found};
        TypeDef -> {'Map', TypeDef}
    end,
    S#r0{ds = [Result | Rest]};

%% type_set: pop type_def, type_name; replace entire type definition
exec(type_set, #r0{ds = [{'Map', TypeDef}, {'Atom', TypeName} | Rest],
                   types = Types} = S) ->
    S#r0{ds = Rest, types = Types#{TypeName => TypeDef}};

%%% === Pattern Matching Primitives (3) ===
%%% Pattern matching is first-class, not derived.

%% match_sig: pop {'List', SigIn}, check remaining stack, push Bool
%% Non-destructive: matched stack items remain
exec(match_sig, #r0{ds = [{'List', SigIn} | Rest]} = S) ->
    Result = match_sig_check(SigIn, Rest),
    S#r0{ds = [{'Bool', Result} | Rest]};

%% match_value: check TOS against expected value, push Bool
%% Non-destructive: TOS remains on stack
exec({match_value, Expected}, #r0{ds = [Actual | _]} = S) ->
    S#r0{ds = [{'Bool', Actual =:= Expected} | S#r0.ds]};

%% select_clause: pop {'List', Clauses}, find first match, execute body
%% Each clause is {SigIn, Body} where Body is a list of instructions
exec(select_clause, #r0{ds = [{'List', Clauses} | Rest]} = S) ->
    case find_matching_clause(Clauses, Rest) of
        {ok, Body} -> run(Body, S#r0{ds = Rest});
        not_found -> error({no_matching_clause, Rest})
    end;

%%% === Actor Primitives (3) ===
%%% Actors are first-class primitives, not library features.

%% spawn_actor: pop {'Code', Body}, spawn process, push {'Actor', Pid}
exec(spawn_actor, #r0{ds = [{'Code', Body} | Rest],
                       words = Words, types = Types} = S) ->
    InitState = #r0{words = Words, types = Types},
    Pid = erlang:spawn(fun() ->
        try run(Body, InitState)
        catch _:_ -> ok
        end
    end),
    S#r0{ds = [{'Actor', Pid} | Rest]};

%% send_msg: pop value, pop actor, send message
exec(send_msg, #r0{ds = [Value, {'Actor', Pid} | Rest]} = S) ->
    Pid ! {r0_msg, Value},
    S#r0{ds = Rest};

%% receive_msg: block until message, push it
exec(receive_msg, #r0{} = S) ->
    receive
        {r0_msg, Value} -> S#r0{ds = [Value | S#r0.ds]}
    end;

%% receive_timeout: pop timeout ms, receive or timeout
exec(receive_timeout, #r0{ds = [{'Int', Ms} | Rest]} = S) ->
    receive
        {r0_msg, Value} ->
            S#r0{ds = [{'Bool', true}, Value | Rest]}
    after Ms ->
        S#r0{ds = [{'Bool', false} | Rest]}
    end;

%%% === I/O Primitives (3) ===

%% emit: append bytes to output buffer
exec({emit, Bytes}, #r0{output = Out} = S) when is_binary(Bytes) ->
    S#r0{output = <<Out/binary, Bytes/binary>>};

%% emit_tos: pop string from stack, append to output
exec(emit_tos, #r0{ds = [{'String', Bytes} | Rest], output = Out} = S) ->
    S#r0{ds = Rest, output = <<Out/binary, Bytes/binary>>};

%% read_n: read N bytes from input buffer, push as String
exec({read_n, N}, #r0{input = In} = S) ->
    case In of
        <<Bytes:N/binary, Rest/binary>> ->
            S#r0{ds = [{'String', Bytes} | S#r0.ds], input = Rest};
        _ ->
            S#r0{ds = [{'Atom', eof} | S#r0.ds]}
    end;

%%% === BIF Call (1) ===
%%% Call any Erlang BIF/function with args from the stack.

exec({bif, Mod, Fun, Arity}, #r0{ds = DS} = S) ->
    {Args, Rest} = take_values(Arity, DS),
    Result = erlang:apply(Mod, Fun, Args),
    S#r0{ds = [auto_tag(Result) | Rest]};

%%% === Map Primitives (5) ===

exec(map_new, #r0{ds = DS} = S) ->
    S#r0{ds = [{'Map', #{}} | DS]};

exec(map_put, #r0{ds = [Value, Key, {'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'Map', Map#{Key => Value}} | Rest]};

exec(map_get, #r0{ds = [Key, {'Map', Map} | Rest]} = S) ->
    case maps:find(Key, Map) of
        {ok, Value} -> S#r0{ds = [Value | Rest]};
        error -> S#r0{ds = [{'Atom', not_found} | Rest]}
    end;

exec(map_delete, #r0{ds = [Key, {'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'Map', maps:remove(Key, Map)} | Rest]};

exec(map_keys, #r0{ds = [{'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'List', maps:keys(Map)} | Rest]};

exec(map_has, #r0{ds = [Key, {'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'Bool', maps:is_key(Key, Map)} | Rest]};

%%% === String Primitives (6) ===
%%% Needed for self-hosted parser.

exec(str_len, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Int', byte_size(Bin)} | Rest]};

exec(str_nth, #r0{ds = [{'Int', N}, {'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', binary:part(Bin, N, 1)} | Rest]};

exec(str_slice, #r0{ds = [{'Int', Len}, {'Int', Start}, {'String', Bin} | Rest]} = S) ->
    ActualLen = min(Len, byte_size(Bin) - Start),
    S#r0{ds = [{'String', binary:part(Bin, Start, max(0, ActualLen))} | Rest]};

exec(str_concat, #r0{ds = [{'String', B}, {'String', A} | Rest]} = S) ->
    S#r0{ds = [{'String', <<A/binary, B/binary>>} | Rest]};

exec(str_eq, #r0{ds = [{'String', B}, {'String', A} | Rest]} = S) ->
    S#r0{ds = [{'Bool', A =:= B} | Rest]};

exec(str_to_int, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    try
        N = binary_to_integer(Bin),
        S#r0{ds = [{'Bool', true}, {'Int', N} | Rest]}
    catch _:_ ->
        S#r0{ds = [{'Bool', false} | Rest]}
    end;

exec(str_to_float, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    try
        F = binary_to_float(Bin),
        S#r0{ds = [{'Bool', true}, {'Float', F} | Rest]}
    catch _:_ ->
        S#r0{ds = [{'Bool', false} | Rest]}
    end;

exec(str_find, #r0{ds = [{'String', Pat}, {'String', Bin} | Rest]} = S) ->
    case binary:match(Bin, Pat) of
        {Pos, _} -> S#r0{ds = [{'Bool', true}, {'Int', Pos} | Rest]};
        nomatch -> S#r0{ds = [{'Bool', false} | Rest]}
    end;

%%% === Type Conversion Primitives (5) ===

exec(to_string, #r0{ds = [{Type, Val} | Rest]} = S) ->
    Str = case Type of
        'String' -> Val;
        'Int' -> integer_to_binary(Val);
        'Float' -> float_to_binary(Val, [{decimals, 10}, compact]);
        'Bool' -> atom_to_binary(Val, utf8);
        'Atom' -> atom_to_binary(Val, utf8);
        _ -> list_to_binary(io_lib:format("~p", [Val]))
    end,
    S#r0{ds = [{'String', Str} | Rest]};

exec(to_int, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Int', binary_to_integer(Bin)} | Rest]};
exec(to_int, #r0{ds = [{'Float', F} | Rest]} = S) ->
    S#r0{ds = [{'Int', trunc(F)} | Rest]};
exec(to_int, #r0{ds = [{'Bool', true} | Rest]} = S) ->
    S#r0{ds = [{'Int', 1} | Rest]};
exec(to_int, #r0{ds = [{'Bool', false} | Rest]} = S) ->
    S#r0{ds = [{'Int', 0} | Rest]};

exec(to_float, #r0{ds = [{'Int', N} | Rest]} = S) ->
    S#r0{ds = [{'Float', float(N)} | Rest]};
exec(to_float, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Float', binary_to_float(Bin)} | Rest]};

exec(to_atom, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Atom', binary_to_atom(Bin, utf8)} | Rest]};

exec(to_bool, #r0{ds = [{'Int', 0} | Rest]} = S) ->
    S#r0{ds = [{'Bool', false} | Rest]};
exec(to_bool, #r0{ds = [{'Int', _} | Rest]} = S) ->
    S#r0{ds = [{'Bool', true} | Rest]};

%%% === Extended String Primitives (6) ===

exec(str_split, #r0{ds = [{'String', Sep}, {'String', Bin} | Rest]} = S) ->
    Parts = binary:split(Bin, Sep, [global]),
    S#r0{ds = [{'List', [{'String', P} || P <- Parts]} | Rest]};

exec(str_contains, #r0{ds = [{'String', Pat}, {'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Bool', binary:match(Bin, Pat) =/= nomatch} | Rest]};

exec(str_starts_with, #r0{ds = [{'String', Pre}, {'String', Bin} | Rest]} = S) ->
    PreLen = byte_size(Pre),
    Result = byte_size(Bin) >= PreLen andalso binary:part(Bin, 0, PreLen) =:= Pre,
    S#r0{ds = [{'Bool', Result} | Rest]};

exec(str_ends_with, #r0{ds = [{'String', Suf}, {'String', Bin} | Rest]} = S) ->
    SufLen = byte_size(Suf),
    BinLen = byte_size(Bin),
    Result = BinLen >= SufLen andalso binary:part(Bin, BinLen - SufLen, SufLen) =:= Suf,
    S#r0{ds = [{'Bool', Result} | Rest]};

exec(str_trim, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', string:trim(Bin)} | Rest]};

exec(str_replace, #r0{ds = [{'String', Rep}, {'String', Pat}, {'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', binary:replace(Bin, Pat, Rep, [global])} | Rest]};

exec(str_upper, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', string:uppercase(Bin)} | Rest]};

exec(str_lower, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', string:lowercase(Bin)} | Rest]};

exec(str_reverse, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'String', list_to_binary(lists:reverse(binary_to_list(Bin)))} | Rest]};

exec(str_substring, #r0{ds = [{'Int', Len}, {'Int', Start}, {'String', Bin} | Rest]} = S) ->
    ActualLen = min(Len, max(0, byte_size(Bin) - Start)),
    S#r0{ds = [{'String', binary:part(Bin, Start, max(0, ActualLen))} | Rest]};

%%% === Extended List Primitives (10) ===

exec(list_len, #r0{ds = [{'List', L} | Rest]} = S) ->
    S#r0{ds = [{'Int', length(L)} | Rest]};

%% Generic length - works on both String and List
exec(generic_len, #r0{ds = [{'String', Bin} | Rest]} = S) ->
    S#r0{ds = [{'Int', byte_size(Bin)} | Rest]};
exec(generic_len, #r0{ds = [{'List', L} | Rest]} = S) ->
    S#r0{ds = [{'Int', length(L)} | Rest]};

exec(list_append, #r0{ds = [{'List', B}, {'List', A} | Rest]} = S) ->
    S#r0{ds = [{'List', A ++ B} | Rest]};

exec(list_reverse, #r0{ds = [{'List', L} | Rest]} = S) ->
    S#r0{ds = [{'List', lists:reverse(L)} | Rest]};

exec(list_nth, #r0{ds = [{'Int', N}, {'List', L} | Rest]} = S) ->
    S#r0{ds = [lists:nth(N + 1, L) | Rest]};  %% 0-indexed

exec(list_last, #r0{ds = [{'List', L} | Rest]} = S) ->
    S#r0{ds = [lists:last(L) | Rest]};

exec(list_take, #r0{ds = [{'Int', N}, {'List', L} | Rest]} = S) ->
    S#r0{ds = [{'List', lists:sublist(L, N)} | Rest]};

exec(list_drop, #r0{ds = [{'Int', N}, {'List', L} | Rest]} = S) ->
    S#r0{ds = [{'List', lists:nthtail(min(N, length(L)), L)} | Rest]};

exec(list_empty, #r0{ds = [{'List', L} | Rest]} = S) ->
    S#r0{ds = [{'Bool', L =:= []} | Rest]};

exec(list_contains, #r0{ds = [Elem, {'List', L} | Rest]} = S) ->
    S#r0{ds = [{'Bool', lists:member(Elem, L)} | Rest]};

exec(list_flatten, #r0{ds = [{'List', L} | Rest]} = S) ->
    Flat = lists:flatmap(fun({'List', Sub}) -> Sub; (E) -> [E] end, L),
    S#r0{ds = [{'List', Flat} | Rest]};

exec(list_zip, #r0{ds = [{'List', B}, {'List', A} | Rest]} = S) ->
    Zipped = [{'List', [X, Y]} || {X, Y} <- lists:zip(A, B)],
    S#r0{ds = [{'List', Zipped} | Rest]};

exec(list_map, #r0{ds = [{'Code', Body}, {'List', L} | Rest]} = S) ->
    Mapped = lists:map(fun(Elem) ->
        S1 = run(Body, S#r0{ds = [Elem]}),
        hd(get_ds(S1))
    end, L),
    S#r0{ds = [{'List', Mapped} | Rest]};

exec(list_filter, #r0{ds = [{'Code', Body}, {'List', L} | Rest]} = S) ->
    Filtered = lists:filter(fun(Elem) ->
        S1 = run(Body, S#r0{ds = [Elem]}),
        case hd(get_ds(S1)) of
            {'Bool', true} -> true;
            _ -> false
        end
    end, L),
    S#r0{ds = [{'List', Filtered} | Rest]};

exec(list_fold, #r0{ds = [{'Code', Body}, Init, {'List', L} | Rest]} = S) ->
    Result = lists:foldl(fun(Elem, Acc) ->
        S1 = run(Body, S#r0{ds = [Elem, Acc]}),
        hd(get_ds(S1))
    end, Init, L),
    S#r0{ds = [Result | Rest]};

%%% === Extended Map Primitives (4) ===

exec(map_values, #r0{ds = [{'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'List', maps:values(Map)} | Rest]};

exec(map_size, #r0{ds = [{'Map', Map} | Rest]} = S) ->
    S#r0{ds = [{'Int', maps:size(Map)} | Rest]};

exec(map_merge, #r0{ds = [{'Map', B}, {'Map', A} | Rest]} = S) ->
    S#r0{ds = [{'Map', maps:merge(A, B)} | Rest]};

exec(map_get_or, #r0{ds = [Default, Key, {'Map', Map} | Rest]} = S) ->
    case maps:find(Key, Map) of
        {ok, Value} -> S#r0{ds = [Value | Rest]};
        error -> S#r0{ds = [Default | Rest]}
    end;

%%% === Tuple Primitives (6) ===

exec(tuple_make, #r0{ds = [{'List', Items} | Rest]} = S) ->
    S#r0{ds = [{'Tuple', list_to_tuple([V || {_, V} <- Items])} | Rest]};

exec(tuple_to_list, #r0{ds = [{'Tuple', T} | Rest]} = S) ->
    S#r0{ds = [{'List', [auto_tag(E) || E <- tuple_to_list(T)]} | Rest]};

exec(tuple_size_op, #r0{ds = [{'Tuple', T} | Rest]} = S) ->
    S#r0{ds = [{'Int', tuple_size(T)} | Rest]};

exec(ok_tuple, #r0{ds = [Val | Rest]} = S) ->
    S#r0{ds = [{'Tuple', {ok, Val}} | Rest]};

exec(error_tuple, #r0{ds = [Val | Rest]} = S) ->
    S#r0{ds = [{'Tuple', {error, Val}} | Rest]};

exec(is_ok, #r0{ds = [{'Tuple', {ok, _}} | _]} = S) ->
    S#r0{ds = [{'Bool', true} | S#r0.ds]};
exec(is_ok, #r0{} = S) ->
    S#r0{ds = [{'Bool', false} | S#r0.ds]};

exec(unwrap_ok, #r0{ds = [{'Tuple', {ok, Val}} | Rest]} = S) ->
    S#r0{ds = [auto_tag(Val) | Rest]};

exec(unwrap_error, #r0{ds = [{'Tuple', {error, Val}} | Rest]} = S) ->
    S#r0{ds = [auto_tag(Val) | Rest]};

%%% === Product Type Primitives (4) ===
%%% Product types: {TypeName, #{field => {Type, Val}}}

exec({product_new, TypeName, Fields}, #r0{ds = DS} = S) ->
    %% Pop field values from stack in reverse order (TOS = last field)
    {FieldVals, Rest} = take_n(length(Fields), DS),
    FieldMap = maps:from_list(lists:zip(Fields, FieldVals)),
    S#r0{ds = [{TypeName, FieldMap} | Rest]};

exec({product_get, Field}, #r0{ds = [{_TypeName, FieldMap} | _] = DS} = S) ->
    Val = maps:get(Field, FieldMap),
    S#r0{ds = [Val | DS]};  %% Non-destructive: instance stays on stack

exec({product_set, Field}, #r0{ds = [Val, {TypeName, FieldMap} | Rest]} = S) ->
    S#r0{ds = [{TypeName, FieldMap#{Field => Val}} | Rest]};

exec(product_fields, #r0{ds = [{_TypeName, FieldMap} | _] = DS} = S) ->
    S#r0{ds = [{'List', maps:keys(FieldMap)} | DS]};

%%% === I/O + Debug Primitives (4) ===

exec(print_tos, #r0{ds = [{Type, Val} | Rest], output = Out} = S) ->
    Str = case Type of
        'String' -> Val;
        'Int' -> integer_to_binary(Val);
        'Float' -> float_to_binary(Val, [{decimals, 10}, compact]);
        'Bool' -> atom_to_binary(Val, utf8);
        'Atom' -> atom_to_binary(Val, utf8);
        _ -> list_to_binary(io_lib:format("~p", [Val]))
    end,
    S#r0{ds = Rest, output = <<Out/binary, Str/binary, "\n">>};

exec(print_stack, #r0{ds = DS, output = Out} = S) ->
    Str = list_to_binary(io_lib:format("~p", [DS])),
    S#r0{output = <<Out/binary, "Stack: ", Str/binary, "\n">>};

exec(assert_true, #r0{ds = [{'Bool', true} | Rest]} = S) ->
    S#r0{ds = Rest};
exec(assert_true, #r0{ds = [Val | _]}) ->
    error({assertion_failed, Val});

exec(assert_eq, #r0{ds = [A, B | Rest]} = S) when A =:= B ->
    S#r0{ds = Rest};
exec(assert_eq, #r0{ds = [A, B | _]}) ->
    error({assertion_failed, expected, B, got, A});

%%% === File I/O Primitives (3) ===

exec(file_read, #r0{ds = [{'String', Path} | Rest]} = S) ->
    case file:read_file(Path) of
        {ok, Content} -> S#r0{ds = [{'Bool', true}, {'String', Content} | Rest]};
        {error, _} -> S#r0{ds = [{'Bool', false} | Rest]}
    end;

exec(file_write, #r0{ds = [{'String', Content}, {'String', Path} | Rest]} = S) ->
    case file:write_file(Path, Content) of
        ok -> S#r0{ds = [{'Bool', true} | Rest]};
        {error, _} -> S#r0{ds = [{'Bool', false} | Rest]}
    end;

exec(file_exists, #r0{ds = [{'String', Path} | Rest]} = S) ->
    S#r0{ds = [{'Bool', filelib:is_file(Path)} | Rest]};

%%% === Logic Primitives (2) ===

exec(and_op, #r0{ds = [{'Bool', A}, {'Bool', B} | Rest]} = S) ->
    S#r0{ds = [{'Bool', A andalso B} | Rest]};

exec(or_op, #r0{ds = [{'Bool', A}, {'Bool', B} | Rest]} = S) ->
    S#r0{ds = [{'Bool', A orelse B} | Rest]};

%%% === Runtime Dispatch ===
%%% For operations not directly mapped to Ring 0 primitives.
%%% Delegates to the existing A4 type system during bootstrap.

exec({apply_impl, OpName}, #r0{ds = DS} = S) ->
    NewStack = af_compile:apply_impl(OpName, DS),
    S#r0{ds = NewStack};

%%% === Word Definition ===

%% def_word (stack-based): pop body and name, define word
exec(def_word, #r0{ds = [{'Code', Body}, {'String', Name} | Rest],
                   words = Words} = S) ->
    Key = if is_binary(Name) -> binary_to_list(Name); true -> Name end,
    S#r0{ds = Rest, words = Words#{Key => Body}};

%% def_word (instruction-parameter): define word with name and body
exec({def_word, Name, Body}, #r0{words = Words} = S) ->
    S#r0{words = Words#{Name => Body}}.

%%% === Helper Functions ===

tag_number(N) when is_float(N) -> {'Float', N};
tag_number(N) -> {'Int', N}.

%% Extract raw values from tagged stack items for BIF calls.
take_values(0, Stack) -> {[], Stack};
take_values(N, [{_, V} | Rest]) ->
    {Vs, Remaining} = take_values(N - 1, Rest),
    {[V | Vs], Remaining}.

%% Auto-tag an Erlang term as a stack value.
auto_tag(N) when is_integer(N) -> {'Int', N};
auto_tag(F) when is_float(F) -> {'Float', F};
auto_tag(true) -> {'Bool', true};
auto_tag(false) -> {'Bool', false};
auto_tag(B) when is_binary(B) -> {'String', B};
auto_tag(L) when is_list(L) -> {'List', [auto_tag(E) || E <- L]};
auto_tag(M) when is_map(M) -> {'Map', M};
auto_tag(T) when is_tuple(T) -> {'Tuple', T};
auto_tag(A) when is_atom(A) -> {'Atom', A};
auto_tag(Other) -> {'Any', Other}.

%% Take N items from stack (for product type construction).
take_n(0, Stack) -> {[], Stack};
take_n(N, [H | Rest]) ->
    {Items, Remaining} = take_n(N - 1, Rest),
    {[H | Items], Remaining}.

%% Check if stack matches a type signature (TOS-first).
%% SigIn entries: atom (type match) | {Type, Value} (value constraint) | 'Any'
match_sig_check([], _Stack) -> true;
match_sig_check(_, []) -> false;
match_sig_check(['Any' | SigRest], [_ | StackRest]) ->
    match_sig_check(SigRest, StackRest);
match_sig_check([{Type, Value} | SigRest], [{Type, Value} | StackRest]) ->
    match_sig_check(SigRest, StackRest);
match_sig_check([Type | SigRest], [{Type, _} | StackRest]) when is_atom(Type) ->
    match_sig_check(SigRest, StackRest);
match_sig_check(_, _) -> false.

find_matching_clause([], _Stack) -> not_found;
find_matching_clause([{SigIn, Body} | Rest], Stack) ->
    case match_sig_check(SigIn, Stack) of
        true -> {ok, Body};
        false -> find_matching_clause(Rest, Stack)
    end.

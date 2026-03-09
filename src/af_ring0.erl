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

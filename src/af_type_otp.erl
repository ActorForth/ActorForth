-module(af_type_otp).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).
-export([compile_gen_server/3]).

%% OTP behaviour generation: compile ActorForth word definitions into
%% proper OTP behaviour modules (gen_server, etc.) that can be dropped
%% into any supervision tree.
%%
%% gen-server-module: ( Atom Atom -- Atom )
%%   Takes a module name and a type name whose words become callbacks.
%%   Generates init/1, handle_call/3, handle_cast/2, terminate/2.
%%   Words with output types become call handlers (sync).
%%   Words without output types (beyond state) become cast handlers (async).

init() ->
    %% gen-server-module: ( Atom(mod_name) Atom(type_name) -- Atom(mod_name) )
    af_type:add_op('Any', #operation{
        name = "gen-server-module",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Atom'],
        impl = fun op_gen_server_module/1
    }),

    ok.

%%% ActorForth word

op_gen_server_module(Cont) ->
    [{'Atom', TypeNameStr}, {'Atom', ModNameStr} | Rest] = Cont#continuation.data_stack,
    ModAtom = list_to_atom(ModNameStr),
    TypeAtom = list_to_atom(TypeNameStr),
    case compile_gen_server(ModAtom, TypeAtom, Cont) of
        {ok, ModAtom} ->
            Cont#continuation{data_stack = [{'Atom', ModNameStr} | Rest]};
        {error, Reason} ->
            error({gen_server_module, Reason})
    end.

%%% Compiler

%% Compile a gen_server module from an ActorForth type and its words.
%% The type's constructor creates the initial state.
%% Each word on the type becomes a handle_call or handle_cast clause.
compile_gen_server(ModAtom, TypeAtom, _Cont) ->
    %% Find all compiled words for this type
    case af_type:get_type(TypeAtom) of
        {ok, #af_type{ops = Ops}} ->
            %% Collect word definitions from the type's dictionary
            WordDefs = collect_word_defs(Ops),
            case WordDefs of
                [] -> {error, {no_words_for_type, TypeAtom}};
                _ ->
                    Forms = gen_server_forms(ModAtom, TypeAtom, WordDefs),
                    case compile:forms(Forms, [binary, return_errors]) of
                        {ok, ModAtom, Binary} ->
                            code:load_binary(ModAtom, atom_to_list(ModAtom) ++ ".beam", Binary),
                            af_word_compiler:store_module_binary(ModAtom, Binary),
                            {ok, ModAtom};
                        {ok, ModAtom, Binary, _Warnings} ->
                            code:load_binary(ModAtom, atom_to_list(ModAtom) ++ ".beam", Binary),
                            af_word_compiler:store_module_binary(ModAtom, Binary),
                            {ok, ModAtom};
                        {error, Errors, _} ->
                            {error, {compile_error, Errors}}
                    end
            end;
        not_found ->
            {error, {unknown_type, TypeAtom}}
    end.

%% Collect {Name, SigIn, SigOut, HasExtraOutputs} for compiled words
collect_word_defs(Ops) ->
    AllOps = lists:flatmap(fun({_Name, OpList}) -> OpList end, maps:to_list(Ops)),
    lists:filtermap(fun(#operation{name = Name, sig_in = SigIn, sig_out = SigOut, source = Source}) ->
        case Source of
            {compiled, _} -> {true, {Name, SigIn, SigOut}};
            {native, _} -> {true, {Name, SigIn, SigOut}};
            _ -> false
        end
    end, AllOps).

%% Generate abstract forms for a gen_server module.
gen_server_forms(ModAtom, TypeAtom, WordDefs) ->
    L = 1,
    %% Classify words: call (returns values beyond state) vs cast (state only)
    {CallWords, CastWords} = classify_words(WordDefs, TypeAtom),

    %% Build handle_call clauses
    CallClauses = [gen_call_clause(Name, SigIn, SigOut, TypeAtom, L) || {Name, SigIn, SigOut} <- CallWords],
    %% Fallback clause for unknown calls
    CallFallback = {clause, L,
        [{var, L, '_Request'}, {var, L, '_From'}, {var, L, 'State'}],
        [],
        [{tuple, L, [{atom, L, reply}, {tuple, L, [{atom, L, error}, {atom, L, unknown_call}]}, {var, L, 'State'}]}]
    },

    %% Build handle_cast clauses
    CastClauses = [gen_cast_clause(Name, SigIn, TypeAtom, L) || {Name, SigIn, _SigOut} <- CastWords],
    CastFallback = {clause, L,
        [{var, L, '_Msg'}, {var, L, 'State'}],
        [],
        [{tuple, L, [{atom, L, noreply}, {var, L, 'State'}]}]
    },

    %% Build exports list
    Exports = [{init, 1}, {handle_call, 3}, {handle_cast, 2}, {terminate, 2}],

    [
        {attribute, L, module, ModAtom},
        {attribute, L, behaviour, gen_server},
        {attribute, L, export, Exports},

        %% init(Args) -> {ok, Args}.
        {function, L, init, 1, [
            {clause, L, [{var, L, 'Args'}], [],
                [{tuple, L, [{atom, L, ok}, {var, L, 'Args'}]}]}
        ]},

        %% handle_call/3
        {function, L, handle_call, 3, CallClauses ++ [CallFallback]},

        %% handle_cast/2
        {function, L, handle_cast, 2, CastClauses ++ [CastFallback]},

        %% terminate(_Reason, _State) -> ok.
        {function, L, terminate, 2, [
            {clause, L, [{var, L, '_Reason'}, {var, L, '_State'}], [],
                [{atom, L, ok}]}
        ]}
    ].

%% Classify words: if output sig has more types than input sig's state type,
%% it returns values -> call. Otherwise -> cast.
classify_words(WordDefs, TypeAtom) ->
    lists:partition(fun({_Name, _SigIn, SigOut}) ->
        %% Call if it produces values beyond just the state type
        StateTypes = [T || T <- SigOut, T =:= TypeAtom],
        length(SigOut) > length(StateTypes)
    end, WordDefs).

%% Generate a handle_call clause for a word that returns values.
%% handle_call({word_name, Arg1, ...}, _From, State) ->
%%     Result = af_otp_dispatch:call(word_name, [Arg1, ...], State),
%%     {reply, {ok, Result}, NewState}.
gen_call_clause(Name, SigIn, SigOut, TypeAtom, L) ->
    NameAtom = list_to_atom(Name),
    %% Args are the non-state types in SigIn
    ArgTypes = [T || T <- SigIn, T =/= TypeAtom, not is_tuple(T) orelse element(1, T) =/= TypeAtom],
    NumArgs = length(ArgTypes),
    ArgVars = [{var, L, list_to_atom("Arg" ++ integer_to_list(I))} || I <- lists:seq(1, NumArgs)],
    %% Request pattern: {word_name, Arg1, Arg2, ...}
    RequestPattern = case ArgVars of
        [] -> {atom, L, NameAtom};
        _ -> {tuple, L, [{atom, L, NameAtom} | ArgVars]}
    end,
    %% Return types (non-state outputs)
    ReturnTypes = [T || T <- SigOut, T =/= TypeAtom, not is_tuple(T) orelse element(1, T) =/= TypeAtom],
    NumReturns = length(ReturnTypes),
    %% Build the dispatch call
    ArgList = build_list(ArgVars, L),
    DispatchCall = {call, L,
        {remote, L, {atom, L, af_otp_dispatch}, {atom, L, call_word}},
        [{string, L, Name}, ArgList, {var, L, 'State'}]},
    %% Pattern match result
    NewStateVar = {var, L, 'NewState'},
    ReturnVar = {var, L, 'ReturnValues'},
    %% {ReturnValues, NewState} = af_otp_dispatch:call_word(...)
    MatchExpr = {match, L,
        {tuple, L, [ReturnVar, NewStateVar]},
        DispatchCall},
    %% Build reply value
    ReplyExpr = case NumReturns of
        1 -> {call, L, {atom, L, hd}, [ReturnVar]};
        _ -> ReturnVar
    end,
    ResultExpr = {tuple, L, [{atom, L, reply},
        {tuple, L, [{atom, L, ok}, ReplyExpr]},
        NewStateVar]},
    {clause, L,
        [RequestPattern, {var, L, '_From'}, {var, L, 'State'}],
        [],
        [MatchExpr, ResultExpr]}.

%% Generate a handle_cast clause for a word that only modifies state.
gen_cast_clause(Name, SigIn, TypeAtom, L) ->
    NameAtom = list_to_atom(Name),
    ArgTypes = [T || T <- SigIn, T =/= TypeAtom, not is_tuple(T) orelse element(1, T) =/= TypeAtom],
    NumArgs = length(ArgTypes),
    ArgVars = [{var, L, list_to_atom("Arg" ++ integer_to_list(I))} || I <- lists:seq(1, NumArgs)],
    MsgPattern = case ArgVars of
        [] -> {atom, L, NameAtom};
        _ -> {tuple, L, [{atom, L, NameAtom} | ArgVars]}
    end,
    ArgList = build_list(ArgVars, L),
    DispatchCall = {call, L,
        {remote, L, {atom, L, af_otp_dispatch}, {atom, L, cast_word}},
        [{string, L, Name}, ArgList, {var, L, 'State'}]},
    {clause, L,
        [MsgPattern, {var, L, 'State'}],
        [],
        [{tuple, L, [{atom, L, noreply}, DispatchCall]}]}.

%% Build an Erlang list abstract form from a list of expressions.
build_list([], L) -> {nil, L};
build_list([H | T], L) -> {cons, L, H, build_list(T, L)}.

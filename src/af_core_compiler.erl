%% af_core_compiler.erl -- Bridge for self-hosted A4 compiler
%%
%% Provides high-level functions callable from A4 via FFI that handle
%% word compilation to BEAM modules. This is the bootstrap helper
%% that will eventually be replaced by pure A4 code.
%%
%% Usage from A4:
%%   "my_mod" "double" compile-word af_core_compiler erlang-call
%%   # or for multiple words:
%%   "my_mod" nil "square" cons "cube" cons compile-words af_core_compiler erlang-apply

-module(af_core_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([
    compile_word/2,
    compile_words/2,
    save_module/2,
    eval_and_compile/3,
    call_compiled/3
]).

-export([init/0]).

%% compile_word(ModName, WordName) -> {ok, ModAtom} | {error, Reason}
%%
%% Look up a compiled word by name and compile it to a BEAM module.
compile_word(ModName, WordName) ->
    ModAtom = to_atom("af_native_" ++ to_str(ModName)),
    NameStr = to_str(WordName),
    WordDefs = af_word_compiler:find_compiled_word_defs(NameStr),
    case WordDefs of
        [] -> {error, {no_word, NameStr}};
        _ -> af_word_compiler:compile_words_to_module(ModAtom, WordDefs)
    end.

%% compile_words(ModName, WordNames) -> {ok, ModAtom} | {error, Reason}
%%
%% Compile multiple words into a single BEAM module.
compile_words(ModName, WordNames) ->
    ModAtom = to_atom("af_native_" ++ to_str(ModName)),
    AllDefs = lists:flatmap(fun(Name) ->
        af_word_compiler:find_compiled_word_defs(to_str(Name))
    end, WordNames),
    case AllDefs of
        [] -> {error, no_words_found};
        _ -> af_word_compiler:compile_words_to_module(ModAtom, AllDefs)
    end.

%% save_module(ModAtom, OutputPath) -> ok | {error, Reason}
%%
%% Write a loaded module's .beam binary to disk.
save_module(ModAtom, OutputPath) ->
    Atom = to_atom(ModAtom),
    case code:get_object_code(Atom) of
        {Atom, Binary, _Filename} ->
            file:write_file(to_str(OutputPath), Binary);
        error ->
            {error, {no_object_code, Atom}}
    end.

%% eval_and_compile(Source, Filename, ModName) -> {ok, ModAtom} | {error, Reason}
%%
%% Parse and interpret A4 source (defining words), then compile all
%% defined words to a BEAM module. This is the core bootstrap operation.
eval_and_compile(Source, Filename, ModName) ->
    try
        %% Parse the source
        Tokens = af_parser:parse(to_str(Source), to_str(Filename)),
        %% Interpret to define words
        Cont = af_interpreter:new_continuation(),
        _FinalCont = af_interpreter:interpret_tokens(Tokens, Cont),
        %% Find all user-defined words (those with compiled bodies)
        AllTypes = af_type:all_types(),
        WordNames = lists:usort(lists:flatmap(fun(T) ->
            maps:fold(fun(Name, Ops, Acc) ->
                case lists:any(fun(#operation{source = S}) ->
                    is_tuple(S) andalso element(1, S) =:= compiled
                end, Ops) of
                    true -> [Name | Acc];
                    false -> Acc
                end
            end, [], T#af_type.ops)
        end, AllTypes)),
        case WordNames of
            [] -> {error, no_words_defined};
            _ ->
                compile_words(ModName, WordNames)
        end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% call_compiled(ModAtom, FunAtom, ArgStack) -> ResultStack
%%
%% Call a compiled A4 function with tagged stack items.
%% ArgStack is a list of {Type, Value} tuples (A4 stack format).
%% Returns the result stack (also tagged).
call_compiled(Mod, Fun, ArgStack) ->
    ModAtom = to_atom(Mod),
    FunAtom = to_atom(Fun),
    Result = erlang:apply(ModAtom, FunAtom, [ArgStack]),
    Result.

%% init/0 -- Register A4 words for the bootstrap compiler
init() ->
    %% call-compiled : Int String String -> Any...
    %% Takes arity, function name, module name from TOS,
    %% then pops N args and calls the compiled A4 function.
    %% Pushes all results back onto the stack.
    af_type:add_op('Any', #operation{
        name = "call-compiled",
        sig_in = ['Int', 'String', 'String'],
        sig_out = ['Any'],
        impl = fun op_call_compiled/1
    }),
    ok.

op_call_compiled(Cont) ->
    io:format("DEBUG call-compiled stack: ~p~n", [Cont#continuation.data_stack]),
    [{'Int', Arity}, {'String', FunBin}, {'String', ModBin} | Rest] =
        Cont#continuation.data_stack,
    ModAtom = to_atom(ModBin),
    FunAtom = to_atom(FunBin),
    %% Pop Arity args from stack (TOS-first, need to reverse for left-to-right)
    {ArgItems, Remaining} = lists:split(Arity, Rest),
    ArgStack = lists:reverse(ArgItems),  % deepest-first = left-to-right
    try
        ResultStack = erlang:apply(ModAtom, FunAtom, [ArgStack]),
        %% Push results back (ResultStack is deepest-first, A4 stack is TOS-first)
        NewStack = lists:reverse(ResultStack) ++ Remaining,
        Cont#continuation{data_stack = NewStack}
    catch
        Class:Reason ->
            Msg = lists:flatten(io_lib:format("~p:~p/1 failed: ~p:~p",
                [ModAtom, FunAtom, Class, Reason])),
            af_error:raise(compile_call_error, Msg, Cont)
    end.

to_str(B) when is_binary(B) -> binary_to_list(B);
to_str(L) when is_list(L) -> L;
to_str(A) when is_atom(A) -> atom_to_list(A).

to_atom(B) when is_binary(B) -> list_to_atom(binary_to_list(B));
to_atom(L) when is_list(L) -> list_to_atom(L);
to_atom(A) when is_atom(A) -> A.

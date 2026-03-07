-module(af_type_any).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0]).

init() ->
    %% dup : Any -> Any, Any
    af_type:add_op('Any', #operation{
        name = "dup", sig_in = ['Any'], sig_out = ['Any', 'Any'],
        impl = fun op_dup/1
    }),

    %% drop : Any ->
    af_type:add_op('Any', #operation{
        name = "drop", sig_in = ['Any'], sig_out = [],
        impl = fun op_drop/1
    }),

    %% swap : Any, Any -> Any, Any
    af_type:add_op('Any', #operation{
        name = "swap", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any'],
        impl = fun op_swap/1
    }),

    %% rot : Any, Any, Any -> Any, Any, Any  (brings 3rd to top)
    af_type:add_op('Any', #operation{
        name = "rot", sig_in = ['Any', 'Any', 'Any'], sig_out = ['Any', 'Any', 'Any'],
        impl = fun op_rot/1
    }),

    %% over : Any, Any -> Any, Any, Any  (copies 2nd to top)
    af_type:add_op('Any', #operation{
        name = "over", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any', 'Any'],
        impl = fun op_over/1
    }),

    %% 2dup : Any, Any -> Any, Any, Any, Any
    af_type:add_op('Any', #operation{
        name = "2dup", sig_in = ['Any', 'Any'], sig_out = ['Any', 'Any', 'Any', 'Any'],
        impl = fun op_2dup/1
    }),

    %% print : Any ->
    af_type:add_op('Any', #operation{
        name = "print", sig_in = ['Any'], sig_out = [],
        impl = fun op_print/1
    }),

    %% stack : ->  (displays stack contents)
    af_type:add_op('Any', #operation{
        name = "stack", sig_in = [], sig_out = [],
        impl = fun op_stack/1
    }),

    %% words : ->  (displays available words)
    af_type:add_op('Any', #operation{
        name = "words", sig_in = [], sig_out = [],
        impl = fun op_words/1
    }),

    %% types : ->  (displays registered types)
    af_type:add_op('Any', #operation{
        name = "types", sig_in = [], sig_out = [],
        impl = fun op_types/1
    }),

    %% see : Atom ->  (display source/definition of a word)
    af_type:add_op('Any', #operation{
        name = "see", sig_in = ['Atom'], sig_out = [],
        impl = fun op_see/1
    }),

    %% assert : Bool ->  (passes silently if true, errors with location if false)
    af_type:add_op('Any', #operation{
        name = "assert", sig_in = ['Bool'], sig_out = [],
        impl = fun op_assert/1
    }),

    %% assert-eq : Any Any ->  (passes if equal type+value, errors with expected/actual)
    af_type:add_op('Any', #operation{
        name = "assert-eq", sig_in = ['Any', 'Any'], sig_out = [],
        impl = fun op_assert_eq/1
    }),

    %% load : String ->  (load and interpret a .a4 file)
    af_type:add_op('Any', #operation{
        name = "load", sig_in = ['String'], sig_out = [],
        impl = fun op_load/1
    }),

    %% import : String -> Atom  (compile .a4 file to BEAM module, load words)
    af_type:add_op('Any', #operation{
        name = "import", sig_in = ['String'], sig_out = ['Atom'],
        impl = fun op_import/1
    }),

    %% compile : String ->  (compile a word to native BEAM function)
    af_type:add_op('Any', #operation{
        name = "compile", sig_in = ['String'], sig_out = [],
        impl = fun op_compile/1
    }),

    %% debug : -> Debug  (pushes Debug marker, handler intercepts on/off)
    af_type:register_type(#af_type{name = 'Debug'}),
    af_type:add_op('Any', #operation{
        name = "debug", sig_in = [], sig_out = ['Debug'],
        impl = fun op_debug/1
    }),
    af_type:register_type(#af_type{
        name = 'Debug',
        ops = get_ops('Debug'),
        handler = fun handle_debug/2
    }),

    ok.

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%%% Operations

op_dup(Cont) ->
    [Top | _] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [Top | Cont#continuation.data_stack]}.

op_drop(Cont) ->
    [_ | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest}.

op_swap(Cont) ->
    [A, B | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [B, A | Rest]}.

op_rot(Cont) ->
    [A, B, C | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [C, A, B | Rest]}.

op_over(Cont) ->
    [A, B | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [B, A, B | Rest]}.

op_2dup(Cont) ->
    [A, B | _] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = [A, B | Cont#continuation.data_stack]}.

op_print(Cont) ->
    [{_Type, Value} | Rest] = Cont#continuation.data_stack,
    io:format("~p~n", [Value]),
    Cont#continuation{data_stack = Rest}.

op_stack(Cont) ->
    Stack = Cont#continuation.data_stack,
    case Stack of
        [] -> io:format("Stack empty~n");
        _ ->
            io:format("Stack(~p):~n", [length(Stack)]),
            lists:foldl(fun({Type, Val}, Idx) ->
                io:format("  ~p) ~p : ~p~n", [Idx, Val, Type]),
                Idx + 1
            end, 0, Stack)
    end,
    Cont.

op_words(Cont) ->
    Types = af_type:all_types(),
    lists:foreach(fun(#af_type{name = Name, ops = Ops}) ->
        case maps:size(Ops) of
            0 -> ok;
            _ ->
                Words = lists:sort(maps:keys(Ops)),
                io:format("~p : ~p~n", [Name, Words])
        end
    end, Types),
    Cont.

op_types(Cont) ->
    Types = [T#af_type.name || T <- af_type:all_types()],
    io:format("Types: ~p~n", [lists:sort(Types)]),
    Cont.

%%% See

op_see(Cont) ->
    [{'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    AllTypes = af_type:all_types(),
    Matches = lists:flatmap(fun(#af_type{name = TypeName, ops = Ops}) ->
        case maps:get(WordName, Ops, []) of
            [] -> [];
            OpList -> [{TypeName, Op} || Op <- OpList]
        end
    end, AllTypes),
    case Matches of
        [] ->
            io:format("Word '~s' not found.~n", [WordName]);
        _ ->
            lists:foreach(fun({TypeName, Op}) ->
                print_word_definition(TypeName, Op)
            end, Matches)
    end,
    Cont#continuation{data_stack = Rest}.

print_word_definition(TypeName, #operation{name = Name, sig_in = SigIn, sig_out = SigOut, source = Source}) ->
    %% Format signature
    SigInStr = format_sig(SigIn),
    SigOutStr = format_sig(SigOut),
    io:format("  : ~s ~s -> ~s ;", [Name, SigInStr, SigOutStr]),
    case Source of
        {compiled, Body} ->
            BodyStr = string:join([Op#operation.name || Op <- Body], " "),
            io:format(" ~s .~n", [BodyStr]);
        {native, Mod} ->
            io:format("  [native: ~p]~n", [Mod]);
        auto ->
            io:format("  [auto-generated]~n");
        _ ->
            io:format("  [built-in]~n")
    end,
    io:format("    in type: ~p~n", [TypeName]).

format_sig([]) -> "";
format_sig(Sig) ->
    %% Sig is TOS-first; display in Forth order (deepest first)
    Reversed = lists:reverse(Sig),
    string:join([format_sig_item(S) || S <- Reversed], " ").

format_sig_item({Type, Value}) ->
    lists:flatten(io_lib:format("~p(~p)", [Type, Value]));
format_sig_item(Type) when is_atom(Type) ->
    atom_to_list(Type).

%%% Assert

op_assert(Cont) ->
    [{'Bool', Val} | Rest] = Cont#continuation.data_stack,
    case Val of
        true ->
            Cont#continuation{data_stack = Rest};
        false ->
            Msg = "Assertion failed: expected True on stack",
            af_error:raise(assertion_failed, Msg, Cont)
    end.

op_assert_eq(Cont) ->
    [Expected, Actual | Rest] = Cont#continuation.data_stack,
    case Expected =:= Actual of
        true ->
            Cont#continuation{data_stack = Rest};
        false ->
            Msg = lists:flatten(io_lib:format(
                "Expected ~s but got ~s",
                [af_error:format_value(Expected), af_error:format_value(Actual)]
            )),
            af_error:raise(assert_eq_failed, Msg, Cont)
    end.

%%% Load

op_load(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    %% Resolve relative paths against the current file's directory
    ResolvedPath = case filename:pathtype(Path) of
        absolute -> Path;
        _ ->
            CurrentFile = case Cont#continuation.current_token of
                #token{file = File} when File =/= "", File =/= "stdin", File =/= "eval" ->
                    filename:dirname(File);
                _ -> "."
            end,
            filename:join(CurrentFile, Path)
    end,
    case file:read_file(ResolvedPath) of
        {ok, Content} ->
            Tokens = af_parser:parse(binary_to_list(Content), ResolvedPath),
            Cont1 = Cont#continuation{data_stack = Rest},
            af_interpreter:interpret_tokens(Tokens, Cont1);
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("Cannot load file ~s: ~p", [ResolvedPath, Reason])),
            af_error:raise(load_error, Msg, Cont)
    end.

%%% Import

op_import(Cont) ->
    [{'String', PathBin} | Rest] = Cont#continuation.data_stack,
    Path = binary_to_list(PathBin),
    %% Resolve relative paths
    ResolvedPath = case filename:pathtype(Path) of
        absolute -> Path;
        _ ->
            CurrentFile = case Cont#continuation.current_token of
                #token{file = File} when File =/= "", File =/= "stdin", File =/= "eval" ->
                    filename:dirname(File);
                _ -> "."
            end,
            filename:join(CurrentFile, Path)
    end,
    case af_compile_file:compile(ResolvedPath) of
        {ok, ModAtom} ->
            Cont#continuation{data_stack = [{'Atom', atom_to_list(ModAtom)} | Rest]};
        {error, Reason} ->
            Msg = lists:flatten(io_lib:format("import failed for ~s: ~p", [ResolvedPath, Reason])),
            af_error:raise(import_error, Msg, Cont)
    end.

%%% Debug

op_debug(Cont) ->
    Cont#continuation{
        data_stack = [{'Debug', #{}} | Cont#continuation.data_stack]
    }.

handle_debug("on", Cont) ->
    [{'Debug', _} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest, debug = true};
handle_debug("off", Cont) ->
    [{'Debug', _} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{data_stack = Rest, debug = false};
handle_debug(Other, _Cont) ->
    error({debug_expected_on_off, Other}).

%%% Compile

op_compile(Cont) ->
    [{'String', NameBin} | Rest] = Cont#continuation.data_stack,
    Name = binary_to_list(NameBin),
    WordDefs = af_word_compiler:find_compiled_word_defs(Name),
    case WordDefs of
        [] ->
            Msg = lists:flatten(io_lib:format("No compiled word '~s' found", [Name])),
            af_error:raise(compile_error, Msg, Cont);
        _ ->
            ModAtom = list_to_atom("af_native_" ++ Name),
            case af_word_compiler:compile_words_to_module(ModAtom, WordDefs) of
                {ok, ModAtom} ->
                    FunAtom = list_to_atom(Name),
                    %% For multi-clause words, use the broadest (type-only) sig
                    %% for the wrapper so it matches any input of the right types.
                    ByType = group_defs_by_type(WordDefs),
                    lists:foreach(fun({TargetType, Defs}) ->
                        BroadSigIn = broadest_sig_in(Defs),
                        {_, _, BroadSigOut, _} = find_broadest_def(Defs),
                        Wrapper = af_word_compiler:make_wrapper(ModAtom, FunAtom, BroadSigIn, BroadSigOut),
                        af_type:replace_ops(TargetType, Name, [Wrapper])
                    end, ByType),
                    Cont#continuation{data_stack = Rest};
                {error, Reason} ->
                    Msg = lists:flatten(io_lib:format("Compile failed for '~s': ~p", [Name, Reason])),
                    af_error:raise(compile_error, Msg, Cont)
            end
    end.

%% Get the broadest sig_in (strip value constraints to bare types).
broadest_sig_in(Defs) ->
    {_, SigIn, _, _} = find_broadest_def(Defs),
    [case S of {T, _V} -> T; T -> T end || S <- SigIn].

%% Find the def with the broadest (no value constraints) sig_in,
%% or fall back to the last def if all have constraints.
find_broadest_def(Defs) ->
    case lists:filter(fun({_, SigIn, _, _}) ->
        not lists:any(fun(S) -> is_tuple(S) end, SigIn)
    end, Defs) of
        [Def | _] -> Def;
        [] -> lists:last(Defs)
    end.

group_defs_by_type(WordDefs) ->
    Groups = lists:foldl(fun({_Name, SigIn, _SigOut, _Body} = Def, Acc) ->
        TargetType = case SigIn of
            [{T, _} | _] -> T;
            [T | _] when is_atom(T) -> T;
            [] -> 'Any'
        end,
        Existing = maps:get(TargetType, Acc, []),
        maps:put(TargetType, Existing ++ [Def], Acc)
    end, #{}, WordDefs),
    maps:to_list(Groups).

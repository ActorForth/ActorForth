-module(af_type_python).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").
-include("af_error.hrl").

-export([init/0, ensure_started/0]).

-ifdef(TEST).
-export([py_to_stack_item/1, af_to_python/1, run_word_with_args/2]).
-endif.

init() ->
    %% py-start : ->
    %% Ensure the Python runtime is started
    af_type:add_op('Any', #operation{
        name = "py-start",
        sig_in = [],
        sig_out = [],
        impl = fun op_py_start/1
    }),

    %% py-call : Int Atom Atom -> Any
    %% Call Python: arg1 ... argN module function arity py-call
    %% Pops N args from stack, calls module.function(*args)
    af_type:add_op('Any', #operation{
        name = "py-call",
        sig_in = ['Int', 'Atom', 'Atom'],
        sig_out = ['Any'],
        impl = fun op_py_call/1
    }),

    %% py-call0 : Atom Atom -> Any
    %% Zero-arg shorthand: module function py-call0
    af_type:add_op('Any', #operation{
        name = "py-call0",
        sig_in = ['Atom', 'Atom'],
        sig_out = ['Any'],
        impl = fun op_py_call0/1
    }),

    %% py-eval : String -> Any
    %% Evaluate a Python expression
    af_type:add_op('Any', #operation{
        name = "py-eval",
        sig_in = ['String'],
        sig_out = ['Any'],
        impl = fun op_py_eval/1
    }),

    %% py-exec : String ->
    %% Execute Python statements (no return value)
    af_type:add_op('Any', #operation{
        name = "py-exec",
        sig_in = ['String'],
        sig_out = [],
        impl = fun op_py_exec/1
    }),

    %% py-register : Atom -> Atom
    %% Register an ActorForth compiled word as a Python-callable function.
    %% The word must have been compiled to native BEAM (via compile-to-beam).
    %% word-name py-register
    af_type:add_op('Any', #operation{
        name = "py-register",
        sig_in = ['Atom'],
        sig_out = ['Atom'],
        impl = fun op_py_register/1
    }),

    %% py-import : String ->
    %% Add a directory to Python's sys.path for importing custom modules
    af_type:add_op('Any', #operation{
        name = "py-import",
        sig_in = ['String'],
        sig_out = [],
        impl = fun op_py_import/1
    }),

    %% py-venv : String ->
    %% Activate a Python virtual environment
    af_type:add_op('Any', #operation{
        name = "py-venv",
        sig_in = ['String'],
        sig_out = [],
        impl = fun op_py_venv/1
    }),

    ok.

ensure_started() ->
    case application:ensure_all_started(erlang_python) of
        {ok, _} -> ok;
        {error, Reason} ->
            error({python_not_available, Reason})
    end.

%%% Operations

op_py_start(Cont) ->
    ensure_started(),
    Cont.

op_py_call(Cont) ->
    ensure_started(),
    [{'Int', Arity}, {'Atom', FunStr}, {'Atom', ModStr} | Rest] = Cont#continuation.data_stack,
    Module = to_bin(ModStr),
    Function = list_to_atom(FunStr),
    case length(Rest) >= Arity of
        true ->
            {ArgItems, Remaining} = lists:split(Arity, Rest),
            ErlArgs = [af_to_python(A) || A <- lists:reverse(ArgItems)],
            try
                {ok, Result} = py:call(Module, Function, ErlArgs),
                StackItem = py_to_stack_item(Result),
                Cont#continuation{data_stack = [StackItem | Remaining]}
            catch
                _:Reason ->
                    Msg = lists:flatten(io_lib:format("py-call ~s:~s/~p failed: ~p",
                        [ModStr, FunStr, Arity, Reason])),
                    af_error:raise(python_error, Msg, Cont)
            end;
        false ->
            af_error:raise(stack_underflow,
                lists:flatten(io_lib:format("py-call needs ~p args on stack", [Arity])),
                Cont)
    end.

op_py_call0(Cont) ->
    ensure_started(),
    [{'Atom', FunStr}, {'Atom', ModStr} | Rest] = Cont#continuation.data_stack,
    Module = to_bin(ModStr),
    Function = list_to_atom(FunStr),
    try
        {ok, Result} = py:call(Module, Function, []),
        StackItem = py_to_stack_item(Result),
        Cont#continuation{data_stack = [StackItem | Rest]}
    catch
        _:Reason ->
            Msg = lists:flatten(io_lib:format("py-call0 ~s:~s failed: ~p",
                [ModStr, FunStr, Reason])),
            af_error:raise(python_error, Msg, Cont)
    end.

op_py_eval(Cont) ->
    ensure_started(),
    [{'String', Expr} | Rest] = Cont#continuation.data_stack,
    try
        {ok, Result} = py:eval(to_bin(Expr)),
        StackItem = py_to_stack_item(Result),
        Cont#continuation{data_stack = [StackItem | Rest]}
    catch
        _:Reason ->
            Msg = lists:flatten(io_lib:format("py-eval failed: ~p", [Reason])),
            af_error:raise(python_error, Msg, Cont)
    end.

op_py_exec(Cont) ->
    ensure_started(),
    [{'String', Code} | Rest] = Cont#continuation.data_stack,
    try
        ok = py:exec(to_bin(Code)),
        Cont#continuation{data_stack = Rest}
    catch
        _:Reason ->
            Msg = lists:flatten(io_lib:format("py-exec failed: ~p", [Reason])),
            af_error:raise(python_error, Msg, Cont)
    end.

op_py_register(Cont) ->
    ensure_started(),
    [{'Atom', WordName} | Rest] = Cont#continuation.data_stack,
    WordAtom = list_to_atom(WordName),
    %% Look up the word in the type registry to find its native module
    case af_word_compiler:find_native_word(WordName) of
        {ok, NativeMod, _Arity, _NumOut} ->
            %% Register the native BEAM function as callable from Python.
            %% Native functions now take/return tagged stack lists,
            %% so we convert raw Python args to tagged items and unwrap the result.
            WrapperFun = fun(Args) ->
                TaggedStack = lists:reverse([af_term:to_stack_item(A) || A <- Args]),
                ResultStack = erlang:apply(NativeMod, WordAtom, [TaggedStack]),
                case ResultStack of
                    [{_Type, Val} | _] -> Val;
                    [] -> ok
                end
            end,
            py:register_function(WordAtom, WrapperFun),
            Cont#continuation{data_stack = [{'Atom', WordName} | Rest]};
        not_found ->
            %% Try to find as a user-defined word and register its interpreted version
            case find_word_op(WordName) of
                {ok, _Op} ->
                    WrapperFun = fun(Args) ->
                        run_word_with_args(WordName, Args)
                    end,
                    py:register_function(WordAtom, WrapperFun),
                    Cont#continuation{data_stack = [{'Atom', WordName} | Rest]};
                not_found ->
                    af_error:raise(python_error,
                        "py-register: word '" ++ WordName ++ "' not found",
                        Cont)
            end
    end.

op_py_import(Cont) ->
    ensure_started(),
    [{'String', Dir} | Rest] = Cont#continuation.data_stack,
    DirStr = to_list(Dir),
    Code = list_to_binary(io_lib:format(
        "import sys\nif '~s' not in sys.path:\n    sys.path.insert(0, '~s')",
        [DirStr, DirStr])),
    ok = py:exec(Code),
    Cont#continuation{data_stack = Rest}.

op_py_venv(Cont) ->
    ensure_started(),
    [{'String', Path} | Rest] = Cont#continuation.data_stack,
    ok = py:activate_venv(to_bin(Path)),
    Cont#continuation{data_stack = Rest}.

%%% Helpers

%% Ensure value is binary (strings may be list or binary on the stack)
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_list(V) -> list_to_binary(V).

to_list(V) when is_list(V) -> V;
to_list(V) when is_binary(V) -> binary_to_list(V).

%% Convert ActorForth stack item to Python-friendly Erlang value
af_to_python({'Int', V}) -> V;
af_to_python({'Float', V}) -> V;
af_to_python({'Bool', V}) -> V;
af_to_python({'String', V}) -> to_bin(V);
af_to_python({'Atom', V}) -> to_bin(V);
af_to_python({'List', Items}) -> [af_to_python(I) || I <- Items];
af_to_python({'Tuple', V}) -> V;
af_to_python({_Type, V}) -> V.

%% Convert Python results to ActorForth stack items
py_to_stack_item(Value) when is_integer(Value) -> {'Int', Value};
py_to_stack_item(Value) when is_float(Value) -> {'Float', Value};
py_to_stack_item(true) -> {'Bool', true};
py_to_stack_item(false) -> {'Bool', false};
py_to_stack_item(none) -> {'Atom', "none"};
py_to_stack_item(Value) when is_binary(Value) -> {'String', binary_to_list(Value)};
py_to_stack_item(Value) when is_list(Value) ->
    {'List', [py_to_stack_item(V) || V <- Value]};
py_to_stack_item(Value) when is_map(Value) ->
    %% Convert Python dict to list of key-value tuples
    Items = maps:fold(fun(K, V, Acc) ->
        [{'Tuple', {py_to_stack_item(K), py_to_stack_item(V)}} | Acc]
    end, [], Value),
    {'List', Items};
py_to_stack_item(Value) when is_tuple(Value) ->
    {'Tuple', Value};
py_to_stack_item(Value) ->
    {'Atom', lists:flatten(io_lib:format("~p", [Value]))}.

%% Find a word operation by name in the type registry
find_word_op(Name) ->
    AllTypes = af_type:all_types(),
    find_word_in_types(Name, AllTypes).

find_word_in_types(_Name, []) -> not_found;
find_word_in_types(Name, [#af_type{name = TypeName} | Rest]) ->
    case af_type:find_op_by_name(Name, TypeName) of
        {ok, Op} -> {ok, Op};
        not_found -> find_word_in_types(Name, Rest)
    end.

%% Run an interpreted word with args from Python
run_word_with_args(WordName, Args) ->
    StackItems = [py_to_stack_item(A) || A <- Args],
    Cont0 = af_interpreter:new_continuation(),
    Cont1 = Cont0#continuation{data_stack = lists:reverse(StackItems)},
    Token = #token{value = WordName, line = 0, column = 0, file = "python"},
    Cont2 = af_interpreter:interpret_token(Token, Cont1),
    case Cont2#continuation.data_stack of
        [Item | _] -> af_term:from_stack_item(Item);
        [] -> none
    end.

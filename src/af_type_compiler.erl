-module(af_type_compiler).

-include("token.hrl").
-include("operation.hrl").
-include("continuation.hrl").
-include("af_type.hrl").

-export([init/0]).

%% Compiler state stored on data_stack as tagged tuples:
%%   {'WordDefinition', #{name => ..., sig_in => ..., sig_out => ..., body => ...}}
%%   {'InputTypeSignature', Map}
%%   {'OutputTypeSignature', Map}
%%   {'CodeCompile', Map}

init() ->
    %% : (colon) — starts a word definition. Registered in Any so it's always available.
    af_type:add_op('Any', #operation{
        name = ":",
        sig_in = [],
        sig_out = [],
        impl = fun op_colon/1
    }),

    %% Register the compiler state types
    af_type:register_type(#af_type{name = 'WordDefinition'}),
    af_type:register_type(#af_type{name = 'InputTypeSignature'}),
    af_type:register_type(#af_type{name = 'OutputTypeSignature'}),
    af_type:register_type(#af_type{name = 'CodeCompile'}),

    %% -> in InputTypeSignature transitions to OutputTypeSignature
    af_type:add_op('InputTypeSignature', #operation{
        name = "->",
        sig_in = ['InputTypeSignature'],
        sig_out = ['OutputTypeSignature'],
        impl = fun op_arrow/1
    }),

    %% ; in OutputTypeSignature transitions to CodeCompile
    af_type:add_op('OutputTypeSignature', #operation{
        name = ";",
        sig_in = ['OutputTypeSignature'],
        sig_out = ['CodeCompile'],
        impl = fun op_semicolon/1
    }),

    %% . in CodeCompile finishes compilation
    af_type:add_op('CodeCompile', #operation{
        name = ".",
        sig_in = ['CodeCompile'],
        sig_out = [],
        impl = fun op_dot/1
    }),

    %% Now set handlers on compiler types (must be after ops are registered)
    init_handlers(),

    ok.

%%% Compiler operations

%% : — Push WordDefinition state onto stack, changing the dictionary context.
op_colon(Cont) ->
    State = #{name => undefined, sig_in => [], sig_out => [], body => []},
    Cont#continuation{
        data_stack = [{'WordDefinition', State} | Cont#continuation.data_stack]
    }.

%%% Type handlers — called by interpreter when no op matches in the type's dict.
%%% This is how compiler types intercept tokens: the handler captures them
%%% instead of the interpreter's default make_atom fallback.
init_handlers() ->
    af_type:register_type(#af_type{
        name = 'WordDefinition',
        ops = get_ops('WordDefinition'),
        handler = fun handle_word_definition/2
    }),
    af_type:register_type(#af_type{
        name = 'InputTypeSignature',
        ops = get_ops('InputTypeSignature'),
        handler = fun handle_input_sig/2
    }),
    af_type:register_type(#af_type{
        name = 'OutputTypeSignature',
        ops = get_ops('OutputTypeSignature'),
        handler = fun handle_output_sig/2
    }),
    af_type:register_type(#af_type{
        name = 'CodeCompile',
        ops = get_ops('CodeCompile'),
        handler = fun handle_code_compile/2
    }).

get_ops(TypeName) ->
    {ok, #af_type{ops = Ops}} = af_type:get_type(TypeName),
    Ops.

%% Handler: WordDefinition
%% If name is undefined, capture this token as the word name and transition
%% to InputTypeSignature.
%% If name is set, shouldn't happen (should have transitioned).
handle_word_definition(TokenValue, Cont) ->
    [{'WordDefinition', State} | Rest] = Cont#continuation.data_stack,
    NewState = State#{name => TokenValue},
    Cont#continuation{
        data_stack = [{'InputTypeSignature', NewState} | Rest]
    }.

%% Handler: InputTypeSignature
%% Accumulate type names into sig_in
handle_input_sig(TokenValue, Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    #{sig_in := SigIn} = State,
    TypeAtom = list_to_atom(TokenValue),
    NewState = State#{sig_in => SigIn ++ [TypeAtom]},
    Cont#continuation{
        data_stack = [{'InputTypeSignature', NewState} | Rest]
    }.

%% Handler: OutputTypeSignature
%% Accumulate type names into sig_out
handle_output_sig(TokenValue, Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    #{sig_out := SigOut} = State,
    TypeAtom = list_to_atom(TokenValue),
    NewState = State#{sig_out => SigOut ++ [TypeAtom]},
    Cont#continuation{
        data_stack = [{'OutputTypeSignature', NewState} | Rest]
    }.

%% Handler: CodeCompile
%% Resolve token to operation reference and append to body
handle_code_compile(TokenValue, Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{sig_in := SigIn, body := Body} = State,
    %% Try to resolve the token as an operation
    %% Build a fake stack based on known input types for resolution
    Op = resolve_compile_token(TokenValue, SigIn),
    NewState = State#{body => Body ++ [Op]},
    Cont#continuation{
        data_stack = [{'CodeCompile', NewState} | Rest]
    }.

%% Compile a token as a late-binding thunk.
%% At runtime, dispatches through the interpreter — handles recursion,
%% forward references, and unknown words (which become Atoms) correctly.
resolve_compile_token(TokenValue, _SigIn) ->
    #operation{
        name = TokenValue,
        sig_in = [],
        sig_out = [],
        impl = fun(Cont) ->
            Token = #token{value = TokenValue},
            af_interpreter:interpret_token(Token, Cont)
        end
    }.

%% -> transition: InputTypeSignature -> OutputTypeSignature
op_arrow(Cont) ->
    [{'InputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{
        data_stack = [{'OutputTypeSignature', State} | Rest]
    }.

%% ; transition: OutputTypeSignature -> CodeCompile
op_semicolon(Cont) ->
    [{'OutputTypeSignature', State} | Rest] = Cont#continuation.data_stack,
    Cont#continuation{
        data_stack = [{'CodeCompile', State} | Rest]
    }.

%% . — Finish compilation: save the word to the appropriate type dictionary
op_dot(Cont) ->
    [{'CodeCompile', State} | Rest] = Cont#continuation.data_stack,
    #{name := Name, sig_in := SigIn0, sig_out := SigOut0, body := Body} = State,

    %% Signatures are accumulated left-to-right (Forth convention: leftmost = deepest).
    %% Reverse so element 0 = TOS for match_sig and dispatch.
    SigIn = lists:reverse(SigIn0),
    SigOut = lists:reverse(SigOut0),

    %% Build the execution function for this compiled word
    Impl = make_word_impl(Body),

    %% Register in the TOS type's dict (first element after reversal).
    TargetType = case SigIn of
        [FirstType | _] -> FirstType;
        [] -> 'Any'
    end,

    %% Create and register the operation
    NewOp = #operation{
        name = Name,
        sig_in = SigIn,
        sig_out = SigOut,
        impl = Impl
    },

    %% Ensure target type exists
    case af_type:get_type(TargetType) of
        not_found -> af_type:register_type(#af_type{name = TargetType});
        _ -> ok
    end,
    af_type:add_op(TargetType, NewOp),

    Cont#continuation{data_stack = Rest}.

%% Build an execution function from a compiled word body.
%% The inner interpreter: fold operations over the continuation.
make_word_impl(Body) ->
    fun(Cont) ->
        execute_body(Body, Cont)
    end.

execute_body([], Cont) -> Cont;
execute_body([#operation{impl = Impl} | Rest], Cont) ->
    NewCont = Impl(Cont),
    execute_body(Rest, NewCont).

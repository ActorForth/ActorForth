-module(af_server).
-behaviour(gen_server).

-include("token.hrl").
-include("continuation.hrl").
-include("af_error.hrl").

-export([start_link/1, start_link/2]).
-export([call/3, cast/3, eval/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    continuation :: #continuation{},
    script_path  :: string(),
    base_depth   :: non_neg_integer()
}).

%% --- Public API ---

-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(ScriptPath) ->
    start_link(ScriptPath, []).

-spec start_link(string(), list()) -> {ok, pid()} | {error, term()}.
start_link(ScriptPath, Opts) ->
    case proplists:get_value(name, Opts) of
        undefined ->
            gen_server:start_link(?MODULE, {ScriptPath}, []);
        Name ->
            gen_server:start_link({local, Name}, ?MODULE, {ScriptPath}, [])
    end.

-spec call(pid() | atom(), string(), [term()]) -> {ok, [term()]} | {error, term()}.
call(Server, WordName, Args) ->
    gen_server:call(Server, {word, WordName, Args}, 10000).

-spec cast(pid() | atom(), string(), [term()]) -> ok.
cast(Server, WordName, Args) ->
    gen_server:cast(Server, {word, WordName, Args}).

-spec eval(pid() | atom(), string()) -> {ok, [term()]} | {error, term()}.
eval(Server, Line) ->
    gen_server:call(Server, {eval, Line}, 10000).

-spec stop(pid() | atom()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% --- gen_server callbacks ---

init({ScriptPath}) ->
    af_repl:init_types(),
    {ok, Content} = file:read_file(ScriptPath),
    Tokens = af_parser:parse(binary_to_list(Content), ScriptPath),
    Cont = af_interpreter:interpret_tokens(Tokens, af_interpreter:new_continuation()),
    BaseDepth = length(Cont#continuation.data_stack),
    {ok, #state{
        continuation = Cont,
        script_path = ScriptPath,
        base_depth = BaseDepth
    }}.

handle_call({word, WordName, Args}, _From, State) ->
    #state{continuation = Cont, base_depth = BaseDepth} = State,
    try
        StackItems = af_term:to_stack_items(Args),
        %% Push args onto stack (first arg deepest, last arg on TOS)
        NewStack = lists:reverse(StackItems) ++ Cont#continuation.data_stack,
        Cont1 = Cont#continuation{data_stack = NewStack},
        Token = #token{value = WordName},
        ResultCont = af_interpreter:interpret_token(Token, Cont1),
        ResultStack = ResultCont#continuation.data_stack,
        %% Pop results: everything above the base depth
        NumResults = length(ResultStack) - BaseDepth,
        case NumResults >= 0 of
            true ->
                {Results, BaseStack} = lists:split(NumResults, ResultStack),
                ConvertedResults = af_term:from_stack_items(Results),
                NewCont = ResultCont#continuation{data_stack = BaseStack},
                {reply, {ok, ConvertedResults}, State#state{continuation = NewCont}};
            false ->
                %% Word consumed more than it produced; keep whatever is left
                ConvertedResults = af_term:from_stack_items(ResultStack),
                NewCont = ResultCont#continuation{data_stack = []},
                {reply, {ok, ConvertedResults}, State#state{continuation = NewCont}}
        end
    catch
        error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call({eval, Line}, _From, State) ->
    #state{continuation = Cont, base_depth = BaseDepth} = State,
    try
        Tokens = af_parser:parse(Line, "eval"),
        ResultCont = af_interpreter:interpret_tokens(Tokens, Cont),
        ResultStack = ResultCont#continuation.data_stack,
        NumResults = length(ResultStack) - BaseDepth,
        case NumResults >= 0 of
            true ->
                {Results, BaseStack} = lists:split(NumResults, ResultStack),
                ConvertedResults = af_term:from_stack_items(Results),
                NewCont = ResultCont#continuation{data_stack = BaseStack},
                {reply, {ok, ConvertedResults}, State#state{continuation = NewCont}};
            false ->
                {reply, {ok, []}, State#state{continuation = ResultCont}}
        end
    catch
        error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({word, WordName, Args}, State) ->
    #state{continuation = Cont} = State,
    try
        StackItems = af_term:to_stack_items(Args),
        NewStack = lists:reverse(StackItems) ++ Cont#continuation.data_stack,
        Cont1 = Cont#continuation{data_stack = NewStack},
        Token = #token{value = WordName},
        ResultCont = af_interpreter:interpret_token(Token, Cont1),
        {noreply, State#state{continuation = ResultCont}}
    catch
        error:_Reason ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

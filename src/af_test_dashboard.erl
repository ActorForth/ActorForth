-module(af_test_dashboard).

-export([start/2, started/4, done/2, stop/1, emit_raw/2]).

%% Live test-run dashboard. Runs as a separate process that owns stdout
%% during a parallel batch. Receives started / done notifications from
%% the runner, keeps rolling state, and redraws a full-screen view at
%% ~10Hz using ANSI escapes.
%%
%% Modes:
%%   dashboard — full-screen redraw (TTY default)
%%   stream    — one line per test completion (CI / non-TTY)
%%   quiet     — only the final summary
%%
%% The renderer is stateful and thread-safe: the runner sends messages,
%% the renderer is the only thing that writes to stdout.

-record(state, {
    mode                  :: dashboard | stream | quiet,
    total     = 0         :: non_neg_integer(),
    passed    = 0         :: non_neg_integer(),
    failed    = 0         :: non_neg_integer(),
    errors    = 0         :: non_neg_integer(),
    skipped   = 0         :: non_neg_integer(),
    running   = #{}       :: map(),   %% Pid => {Group, Name, StartTime}
    completed = []        :: list(),
    t0        = 0         :: integer(),
    refresh   = 100       :: non_neg_integer()  %% ms between redraws
}).

%%% Public API

%% Start a renderer. Mode: dashboard | stream | quiet. Total: expected test count.
start(Mode, Total) ->
    Self = self(),
    Pid = spawn_link(fun() ->
        init(Mode, Total, Self)
    end),
    Pid.

started(Dashboard, Pid, Group, Name) when is_pid(Dashboard) ->
    Dashboard ! {started, Pid, Group, Name},
    ok.

done(Dashboard, Result) when is_pid(Dashboard) ->
    Dashboard ! {done, Result},
    ok.

%% Ask the dashboard to print an arbitrary line (used for boot messages).
emit_raw(Dashboard, Line) when is_pid(Dashboard) ->
    Dashboard ! {raw, Line},
    ok.

%% Stop the dashboard; renderer sends final summary and exits.
stop(Dashboard) when is_pid(Dashboard) ->
    Ref = make_ref(),
    Dashboard ! {stop, self(), Ref},
    receive
        {stopped, Ref, Counts} -> Counts
    after 5000 ->
        exit(Dashboard, kill),
        {0, 0, 0}
    end.

%%% Renderer

init(Mode, Total, _Parent) ->
    State = #state{
        mode = Mode, total = Total,
        t0 = erlang:monotonic_time(millisecond)
    },
    case Mode of
        dashboard ->
            %% Clear screen, hide cursor.
            io:put_chars("\e[2J\e[?25l"),
            render(State);
        _ -> ok
    end,
    loop(State, fresh_timer(State)).

fresh_timer(#state{mode = dashboard, refresh = Ms}) ->
    erlang:send_after(Ms, self(), tick);
fresh_timer(_) -> undefined.

loop(State, Timer) ->
    receive
        {started, Pid, Group, Name} ->
            Running = maps:put(Pid, {Group, Name, now_ms()}, State#state.running),
            loop(State#state{running = Running}, Timer);
        {done, Result} ->
            NewState = handle_done(Result, State),
            case State#state.mode of
                stream     -> emit_line_stream(Result);
                quiet      -> ok;
                dashboard  -> ok  %% tick will redraw
            end,
            loop(NewState, Timer);
        {raw, Line} ->
            io:put_chars(Line),
            loop(State, Timer);
        tick ->
            render(State),
            NewTimer = fresh_timer(State),
            loop(State, NewTimer);
        {stop, Parent, Ref} ->
            case Timer of
                undefined -> ok;
                T         -> erlang:cancel_timer(T)
            end,
            final_render(State),
            Counts = {State#state.passed, State#state.failed, State#state.errors},
            Parent ! {stopped, Ref, Counts}
    end.

handle_done(#{status := pass, pid := Pid}, State) ->
    Running = maps:remove(Pid, State#state.running),
    State#state{passed = State#state.passed + 1, running = Running};
handle_done(#{status := skip, pid := Pid}, State) ->
    Running = maps:remove(Pid, State#state.running),
    State#state{skipped = State#state.skipped + 1, running = Running};
handle_done(#{status := fail, pid := Pid} = R, State) ->
    Running = maps:remove(Pid, State#state.running),
    State#state{failed = State#state.failed + 1,
                running = Running,
                completed = [R | State#state.completed]};
handle_done(#{status := load_error} = R, State) ->
    State#state{errors = State#state.errors + 1,
                completed = [R | State#state.completed]}.

%%% Rendering

render(#state{mode = dashboard} = S) ->
    Done = S#state.passed + S#state.failed + S#state.errors,
    Elapsed = (now_ms() - S#state.t0) / 1000,
    Buf = [
        "\e[H",  %% home
        "\e[J",  %% clear to end of screen
        header(),
        counts_line(S, Done),
        running_block(S),
        progress_bar(Done, S#state.total),
        footer(Elapsed)
    ],
    io:put_chars(Buf);
render(_) -> ok.

final_render(#state{mode = dashboard} = S) ->
    %% Leave the final summary on the screen and restore cursor.
    Done = S#state.passed + S#state.failed + S#state.errors,
    Elapsed = (now_ms() - S#state.t0) / 1000,
    io:put_chars([
        "\e[H\e[J",
        header(),
        counts_line(S, Done),
        "\n  ", progress_bar(Done, S#state.total), "\n",
        footer(Elapsed),
        "\e[?25h"  %% show cursor
    ]);
final_render(#state{mode = stream} = S) ->
    Done = S#state.passed + S#state.failed + S#state.errors + S#state.skipped,
    Elapsed = (now_ms() - S#state.t0) / 1000,
    io:format("~n==========~n"),
    io:format("~b passed  ~b failed  ~b errors  ~b skipped   (~b / ~b in ~.2fs)~n~n",
              [S#state.passed, S#state.failed, S#state.errors, S#state.skipped,
               Done, S#state.total, Elapsed]);
final_render(#state{mode = quiet} = S) ->
    Done = S#state.passed + S#state.failed + S#state.errors + S#state.skipped,
    io:format("~b/~b tests passed, ~b failed, ~b errors, ~b skipped~n",
              [S#state.passed, Done, S#state.failed, S#state.errors,
               S#state.skipped]).

header() ->
    "  ActorForth Test Runner\n"
    "  " ++ lists:duplicate(70, $=) ++ "\n\n".

counts_line(S, Done) ->
    io_lib:format("  \e[32m~b passed\e[0m   \e[31m~b failed\e[0m   \e[33m~b errors\e[0m"
                  "   (~b / ~b)\n\n",
                  [S#state.passed, S#state.failed, S#state.errors,
                   Done, S#state.total]).

running_block(#state{running = R}) when map_size(R) =:= 0 ->
    "";
running_block(#state{running = R}) ->
    Entries = maps:to_list(R),
    Now = now_ms(),
    Header = io_lib:format("  Running now (~b):\n", [length(Entries)]),
    Lines = [io_lib:format("    ~p  ~s/~s  ~bms\n",
                           [Pid, group_str(G), N, Now - T])
             || {Pid, {G, N, T}} <- lists:sublist(Entries, 8)],
    [Header | Lines] ++ "\n".

progress_bar(Done, 0) ->
    io_lib:format("  [~s] ~b/~b~n", [lists:duplicate(40, $ ), Done, 0]);
progress_bar(Done, Total) ->
    Width = 40,
    Filled = (Done * Width) div max(Total, 1),
    Empty  = Width - Filled,
    Bar = lists:duplicate(Filled, $=) ++ lists:duplicate(Empty, $ ),
    Pct = (Done * 100) div max(Total, 1),
    io_lib:format("  [~s] ~b% (~b / ~b)~n", [Bar, Pct, Done, Total]).

footer(Elapsed) ->
    io_lib:format("  elapsed: ~.2fs~n", [Elapsed]).

emit_line_stream(#{status := pass, group := Group, name := Name,
                   duration_us := Us, pid := Pid}) ->
    io:format("\e[32mok\e[0m     ~p ~s/~s  ~bus~n",
              [Pid, group_str(Group), Name, Us]);
emit_line_stream(#{status := skip, group := Group, name := Name,
                   reason := Reason}) ->
    io:format("\e[34mskip\e[0m   ~s/~s  (~s)~n",
              [group_str(Group), Name, Reason]);
emit_line_stream(#{status := fail, group := Group, name := Name,
                   duration_us := Us, pid := Pid, reason := Reason} = R) ->
    io:format("\e[31mnot ok\e[0m ~p ~s/~s  ~bus~n  ~p~n",
              [Pid, group_str(Group), Name, Us, Reason]),
    case maps:get(diagnosis, R, undefined) of
        undefined -> ok;
        Diag when is_map(Diag) -> emit_diagnosis(Diag);
        Other -> io:format("  \e[35mdiagnosis:\e[0m ~p~n", [Other])
    end;
emit_line_stream(#{status := load_error, name := Name, reason := Reason}) ->
    io:format("\e[33mLOAD ERROR\e[0m ~s~n  ~p~n", [Name, Reason]).

group_str([]) -> "(top)";
group_str(Path) -> string:join([binary_to_list(B) || B <- Path], "/").

emit_diagnosis(#{category := Cat, message := Msg} = Diag) ->
    MsgStr = lists:flatten(io_lib:format("~ts", [Msg])),
    io:format("  \e[35m~s:\e[0m ~ts~n", [atom_to_list(Cat), MsgStr]),
    case maps:get(location, Diag, undefined) of
        undefined -> ok;
        #{file := F, line := L, column := C, token := T} ->
            io:format("    at ~ts:~b:~b (token: ~ts)~n", [F, L, C, T])
    end.

now_ms() -> erlang:monotonic_time(millisecond).

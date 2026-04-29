%% af_gol - Game of Life support: grid wiring, pattern placement, render.
%%
%% The interesting parts of the demo (cell actor, tick loop) live in a4.
%% This module handles the boring graph math (toroidal neighbours,
%% pattern centering) and the I/O-heavy render loop. Cells are stored
%% as a flat list of {'Actor', ...} stack items, row-major (idx = R*W + C).

-module(af_gol).

-export([wire_grid/3, place_pattern/4, snapshot_all/1, count_alive/1]).
-export([spawn_grid/2, build_game/3, render/4, kill_grid/1]).
-export([notify_cast/2]).

%% ---------- spawn ----------

%% spawn_grid(W, H) -> [CellActor]
%% Spawns W*H Cell actors, all initially dead with no neighbours wired.
spawn_grid(W, H) ->
    N = W * H,
    [spawn_cell() || _ <- lists:seq(1, N)].

spawn_cell() ->
    TypeName = 'Cell',
    %% {Cell, alive=false, received-count=0, neighbours=[]}
    Instance = {'Cell', false, 0, []},
    Vocab0 = af_type_actor:build_vocab(TypeName),
    Vocab = Vocab0#{"stop" => [#{args => [], returns => []}]},
    Cache = af_actor_worker:build_native_cache(TypeName),
    LoopFun = fun() -> af_type_actor:actor_loop(TypeName, Instance, Cache) end,
    Pid = erlang:spawn_opt(LoopFun, [link, {min_heap_size, 100000}]),
    {'Actor', #{pid => Pid, type_name => TypeName, vocab => Vocab}}.

%% ---------- one-shot build ----------

%% build_game(W, H, Pattern) -> [CellActor]
%% Spawns the grid, wires toroidal neighbours, places the pattern.
%% Returns the cell list so a4 can wrap it in a Game struct.
build_game(W, H, Pattern) ->
    Cells = spawn_grid(W, H),
    wire_grid(W, H, Cells),
    place_pattern(W, H, Cells, Pattern),
    %% Return the full Game flat-tuple so to_stack_item recognises it as
    %% a Game product instance (Game has 4 fields, this tuple has 5
    %% elements: tag + 4). Returning just Cells would lose the wrapper
    %% and round-trip each Actor through {'Tuple', ...}.
    {'Game', W, H, Cells, 0}.

%% ---------- wire ----------

wire_grid(W, H, Cells) when is_list(Cells), length(Cells) =:= W * H ->
    Arr = list_to_tuple(Cells),
    lists:foreach(
        fun(Idx) ->
            Cell = element(Idx + 1, Arr),
            Ns = neighbours(Idx, W, H, Arr),
            cast(Cell, "wire", [{'List', Ns}])
        end,
        lists:seq(0, W * H - 1)),
    ok.

%% ---------- place ----------

place_pattern(W, H, Cells, Pattern) when is_list(Cells), length(Cells) =:= W * H ->
    {PatW, PatH} = pattern_bounds(Pattern),
    OffX = (W - PatW) div 2,
    OffY = (H - PatH) div 2,
    Arr = list_to_tuple(Cells),
    lists:foreach(
        fun(Pos) ->
            {PX, PY} = pos_xy(Pos),
            X = wrap(OffX + PX, W),
            Y = wrap(OffY + PY, H),
            Idx = Y * W + X,
            cast(element(Idx + 1, Arr), "set-alive", [{'Bool', true}])
        end,
        Pattern),
    ok.

%% ---------- snapshot for renderer ----------

snapshot_all(Cells) ->
    Self = self(),
    Refs = [begin
                Ref = make_ref(),
                pid_of(C) ! {call, "state", [], Self, Ref},
                Ref
            end || C <- Cells],
    [receive {reply, R, [{'Bool', V}]} -> V
     after 5000 -> error({snapshot_timeout, R}) end
     || R <- Refs].

%% Cells arrive either as full {'Actor', Map} stack items (when called
%% from Erlang directly) or as raw Pids (when round-tripped through the
%% FFI's from_stack_item conversion). Both work for messaging.
pid_of(P) when is_pid(P) -> P;
pid_of({'Actor', #{pid := P}}) -> P.

count_alive(Bools) ->
    lists:foldl(fun(true, N) -> N + 1; (_, N) -> N end, 0, Bools).

%% ---------- render ----------

%% render(W, H, Cells, Gen) - paints the grid at the top of the terminal
%% using half-block characters and prints a stats line below.
render(W, H, Cells, Gen) ->
    States = snapshot_all(Cells),
    Arr = list_to_tuple(States),
    NumPairs = (H + 1) div 2,
    Rows = [render_row(R, W, H, Arr) || R <- lists:seq(0, NumPairs - 1)],
    %% One io call per frame keeps output coherent and reduces flicker.
    Alive = count_alive(States),
    Stats = io_lib:format("\e[~B;1H\e[2KGen ~B  Alive ~B / ~B  ~Bx~B",
                          [NumPairs + 1, Gen, Alive, W * H, W, H]),
    io:put_chars(["\e[H", Rows, Stats]),
    ok.

render_row(R, W, H, Arr) ->
    TopRow = R * 2,
    BotRow = R * 2 + 1,
    HasBot = BotRow < H,
    Cells = [pixel(state_at(Arr, TopRow, C, W),
                   case HasBot of
                       true  -> state_at(Arr, BotRow, C, W);
                       false -> false
                   end)
             || C <- lists:seq(0, W - 1)],
    [io_lib:format("\e[~B;1H", [R + 1]), Cells].

state_at(Arr, R, C, W) ->
    element(R * W + C + 1, Arr).

pixel(true, true)   -> "█";
pixel(true, false)  -> "▀";
pixel(false, true)  -> "▄";
pixel(false, false) -> $\s.

%% ---------- shutdown ----------

kill_grid(Cells) ->
    lists:foreach(fun(C) ->
        Pid = pid_of(C),
        catch unlink(Pid),
        catch exit(Pid, shutdown)
    end, Cells),
    ok.

%% ---------- internals ----------

cast(C, WordName, Args) ->
    pid_of(C) ! {cast, WordName, Args},
    ok.

%% notify_cast(Actor, Bool) - one-liner that casts {accumulate, Bool}.
%% Used by Cell.notify so we don't have to inject a variable bool into
%% an `<< accumulate >>` block (the language has no clean way to push
%% a runtime value into the local stack of a send block).
notify_cast(C, Bool) ->
    pid_of(C) ! {cast, "accumulate", [{'Bool', Bool}]},
    ok.

neighbours(Idx, W, H, Arr) ->
    Y = Idx div W,
    X = Idx rem W,
    [ element(NIdx + 1, Arr)
      || {DX, DY} <- offsets(),
         NX <- [wrap(X + DX, W)],
         NY <- [wrap(Y + DY, H)],
         NIdx <- [NY * W + NX] ].

offsets() ->
    [ {DX, DY}
      || DX <- [-1, 0, 1],
         DY <- [-1, 0, 1],
         not (DX =:= 0 andalso DY =:= 0) ].

wrap(N, M) ->
    ((N rem M) + M) rem M.

pattern_bounds([]) -> {0, 0};
pattern_bounds(Pattern) ->
    XS = [PX || P <- Pattern, {PX, _} <- [pos_xy(P)]],
    YS = [PY || P <- Pattern, {_, PY} <- [pos_xy(P)]],
    {lists:max(XS) + 1, lists:max(YS) + 1}.

%% Pos arrives either as a flat product tuple {'Pos', PX, PY} (when
%% to_stack_item passes through) or as a map (when from_stack_item
%% converted it across the FFI boundary).
pos_xy({'Pos', PX, PY}) -> {PX, PY};
pos_xy(#{px := PX, py := PY}) -> {PX, PY};
pos_xy(Other) -> error({bad_pos, Other}).

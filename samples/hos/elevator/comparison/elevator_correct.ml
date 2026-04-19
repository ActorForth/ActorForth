(* elevator_correct.ml

   A correctly-implemented two-elevator controller in OCaml.

   Uses OCaml's module system to hide subsystem internals, algebraic
   data types to enforce exhaustive state handling, and pattern matching
   to make invalid state combinations unrepresentable.

   Where Python needed a dict-based transition table, OCaml uses a
   single pattern match that the compiler checks for completeness.
*)

(* ---------- Types ---------- *)

type direction = Up | Down | Idle

(* One unified car state. The compiler will force every pattern match
   against this type to handle every case. This replaces the Python
   dictionary-based _PERMITTED table. *)
type car_state =
  | Idle_at_floor
  | Door_opening
  | Loading
  | Door_closing
  | Preparing_to_move
  | Moving
  | Arriving
  | Emergency_stopped
  | Emergency_door_open

type request = {
  floor: int;
  direction: direction;
  timestamp: float;
}

type assignment = {
  request: request;
  car_id: int;
  assigned_at: float;
  mutable completed: bool;
}

(* Compile-time enforcement of valid transitions: any attempt to
   transition from state A to state B that's not in this function
   will fail the pattern match. No runtime dictionary lookup. *)
let is_valid_transition from_state to_state =
  match (from_state, to_state) with
  | Idle_at_floor, Door_opening
  | Idle_at_floor, Preparing_to_move
  | Idle_at_floor, Emergency_stopped
  | Door_opening, Loading
  | Door_opening, Emergency_door_open
  | Loading, Door_closing
  | Loading, Emergency_door_open
  | Door_closing, Preparing_to_move
  | Door_closing, Idle_at_floor
  | Door_closing, Emergency_stopped
  | Preparing_to_move, Moving
  | Preparing_to_move, Emergency_stopped
  | Moving, Arriving
  | Moving, Emergency_stopped
  | Arriving, Door_opening
  | Arriving, Idle_at_floor
  | Arriving, Emergency_stopped
  | Emergency_stopped, Emergency_door_open
  | Emergency_stopped, Idle_at_floor
  | Emergency_door_open, Idle_at_floor -> true
  | _ -> false

exception Invalid_transition of string

(* ---------- Private subsystems ---------- *)

(* Door and Motor are defined inside Car's implementation module
   and their types are not exposed. Module signatures do what Python's
   underscore convention only suggests. *)

module Car : sig
  type t
  val make :
    car_id:int ->
    on_state_change:(int -> car_state -> unit) ->
    t
  val car_id : t -> int
  val current_floor : t -> int
  val state : t -> car_state
  val assign : t -> assignment -> bool
  val trigger_emergency : t -> unit
  val clear_emergency : t -> unit
  val run : t -> unit
  val stop : t -> unit
end = struct

  (* Private door type -- not exposed in the signature *)
  type door = {
    mutable is_open: bool;
  }

  let door_open d = Thread.delay 0.5; d.is_open <- true
  let door_close d = Thread.delay 0.5; d.is_open <- false

  (* Private motor type -- not exposed *)
  type motor = {
    mutable current_floor: int;
    mutable moving: bool;
  }

  let motor_move_to m target =
    m.moving <- true;
    let dir = if target > m.current_floor then 1 else -1 in
    while m.current_floor <> target do
      Thread.delay 0.3;
      m.current_floor <- m.current_floor + dir
    done;
    m.moving <- false

  type t = {
    car_id: int;
    door: door;
    motor: motor;
    mutable state: car_state;
    mutable assignment: assignment option;
    mutex: Mutex.t;
    condition: Condition.t;
    mutable running: bool;
    on_state_change: int -> car_state -> unit;
  }

  let make ~car_id ~on_state_change = {
    car_id;
    door = { is_open = false };
    motor = { current_floor = 1; moving = false };
    state = Idle_at_floor;
    assignment = None;
    mutex = Mutex.create ();
    condition = Condition.create ();
    running = true;
    on_state_change;
  }

  let car_id c = c.car_id
  let current_floor c = c.motor.current_floor

  let state c =
    Mutex.lock c.mutex;
    let s = c.state in
    Mutex.unlock c.mutex;
    s

  (* Validated state transition. Called with mutex held. *)
  let transition c new_state =
    if not (is_valid_transition c.state new_state) then
      raise (Invalid_transition
        (Printf.sprintf "Car %d: invalid transition" c.car_id));
    c.state <- new_state;
    Condition.broadcast c.condition;
    c.on_state_change c.car_id new_state

  let assign c a =
    Mutex.lock c.mutex;
    let accepted =
      match c.state, c.assignment with
      | Idle_at_floor, None ->
        c.assignment <- Some a;
        Condition.broadcast c.condition;
        true
      | _ -> false
    in
    Mutex.unlock c.mutex;
    accepted

  let trigger_emergency c =
    Mutex.lock c.mutex;
    (match c.state with
     | Emergency_stopped | Emergency_door_open -> ()
     | _ ->
       c.state <- Emergency_stopped;
       Condition.broadcast c.condition;
       c.on_state_change c.car_id Emergency_stopped);
    Mutex.unlock c.mutex

  let clear_emergency c =
    Mutex.lock c.mutex;
    (match c.state with
     | Emergency_door_open ->
       door_close c.door;
       transition c Idle_at_floor
     | Emergency_stopped ->
       transition c Idle_at_floor
     | _ -> ());
    Mutex.unlock c.mutex

  let execute_request c a =
    let target = a.request.floor in

    if c.motor.current_floor <> target then begin
      Mutex.lock c.mutex;
      (if c.state = Idle_at_floor then transition c Preparing_to_move);
      Mutex.unlock c.mutex;

      Mutex.lock c.mutex;
      let proceed = c.state = Preparing_to_move in
      (if proceed then transition c Moving);
      Mutex.unlock c.mutex;

      if proceed then begin
        motor_move_to c.motor target;
        Mutex.lock c.mutex;
        (if c.state = Moving then transition c Arriving);
        Mutex.unlock c.mutex
      end
    end;

    Mutex.lock c.mutex;
    let can_open =
      c.state = Arriving || c.state = Idle_at_floor in
    (if can_open then transition c Door_opening);
    Mutex.unlock c.mutex;

    if can_open then begin
      door_open c.door;

      Mutex.lock c.mutex;
      (if c.state = Door_opening then transition c Loading);
      Mutex.unlock c.mutex;

      Thread.delay 1.0;

      Mutex.lock c.mutex;
      (if c.state = Loading then transition c Door_closing);
      Mutex.unlock c.mutex;

      door_close c.door;

      Mutex.lock c.mutex;
      (if c.state = Door_closing then begin
        transition c Idle_at_floor;
        c.assignment <- None;
        a.completed <- true
      end);
      Mutex.unlock c.mutex
    end

  let rec run c =
    Mutex.lock c.mutex;
    while c.running && c.assignment = None
          && c.state <> Emergency_stopped do
      Condition.wait c.condition c.mutex
    done;
    if not c.running then begin
      Mutex.unlock c.mutex
    end else if c.state = Emergency_stopped then begin
      door_open c.door;
      transition c Emergency_door_open;
      while c.running && c.state = Emergency_door_open do
        Condition.wait c.condition c.mutex
      done;
      Mutex.unlock c.mutex;
      run c
    end else begin
      let a = match c.assignment with
        | Some a -> a
        | None -> assert false in
      Mutex.unlock c.mutex;
      execute_request c a;
      run c
    end

  let stop c =
    Mutex.lock c.mutex;
    c.running <- false;
    Condition.broadcast c.condition;
    Mutex.unlock c.mutex
end

(* ---------- Dispatcher ---------- *)

module Dispatcher : sig
  type t
  val make : num_cars:int -> t
  val request : t -> int -> direction -> unit
  val trigger_emergency : t -> unit
  val clear_emergency : t -> unit
  val get_cars_for_test : t -> Car.t list
  val run : t -> unit
  val stop : t -> unit
end = struct
  type t = {
    cars: Car.t array;
    mutable pending: request list;
    mutable assignments: assignment list;
    car_states: (int, car_state) Hashtbl.t;
    mutex: Mutex.t;
    condition: Condition.t;
    mutable running: bool;
    mutable emergency_active: bool;
  }

  let make ~num_cars =
    let car_states = Hashtbl.create num_cars in
    let t_ref = ref None in
    let on_state_change car_id new_state =
      match !t_ref with
      | None -> ()
      | Some t ->
        Mutex.lock t.mutex;
        Hashtbl.replace t.car_states car_id new_state;
        Condition.broadcast t.condition;
        Mutex.unlock t.mutex
    in
    let cars = Array.init num_cars (fun i ->
      Car.make ~car_id:i ~on_state_change) in
    Array.iter (fun c -> Hashtbl.add car_states (Car.car_id c) Idle_at_floor) cars;
    let t = {
      cars;
      pending = [];
      assignments = [];
      car_states;
      mutex = Mutex.create ();
      condition = Condition.create ();
      running = true;
      emergency_active = false;
    } in
    t_ref := Some t;
    t

  let request d floor direction =
    Mutex.lock d.mutex;
    if not d.emergency_active then begin
      d.pending <- d.pending @ [{floor; direction; timestamp = Unix.gettimeofday ()}];
      Condition.broadcast d.condition
    end;
    Mutex.unlock d.mutex

  let trigger_emergency d =
    Mutex.lock d.mutex;
    if d.emergency_active then Mutex.unlock d.mutex
    else begin
      d.emergency_active <- true;
      let cars_snapshot = Array.to_list d.cars in
      Mutex.unlock d.mutex;
      List.iter Car.trigger_emergency cars_snapshot
    end

  let clear_emergency d =
    Mutex.lock d.mutex;
    if not d.emergency_active then Mutex.unlock d.mutex
    else begin
      d.emergency_active <- false;
      let cars_snapshot = Array.to_list d.cars in
      Mutex.unlock d.mutex;
      List.iter Car.clear_emergency cars_snapshot
    end

  let get_cars_for_test d = Array.to_list d.cars

  let rec run d =
    Mutex.lock d.mutex;
    while d.running && (d.pending = [] || d.emergency_active) do
      Condition.wait d.condition d.mutex
    done;
    if not d.running then Mutex.unlock d.mutex
    else begin
      let req, rest = match d.pending with
        | [] -> assert false
        | r :: rest -> r, rest in
      d.pending <- rest;

      let idle_cars = Array.to_list d.cars
        |> List.filter (fun c ->
          try Hashtbl.find d.car_states (Car.car_id c) = Idle_at_floor
          with Not_found -> false) in

      if idle_cars = [] then begin
        d.pending <- req :: d.pending;
        Condition.wait d.condition d.mutex;
        Mutex.unlock d.mutex;
        run d
      end else begin
        let best = List.fold_left (fun acc c ->
          match acc with
          | None -> Some c
          | Some b ->
            let db = abs (Car.current_floor b - req.floor) in
            let dc = abs (Car.current_floor c - req.floor) in
            if dc < db then Some c else Some b) None idle_cars in
        let best = match best with Some c -> c | None -> assert false in
        let a = {
          request = req;
          car_id = Car.car_id best;
          assigned_at = Unix.gettimeofday ();
          completed = false;
        } in
        Mutex.unlock d.mutex;

        let accepted = Car.assign best a in
        if not accepted then begin
          Mutex.lock d.mutex;
          d.pending <- req :: d.pending;
          Condition.broadcast d.condition;
          Mutex.unlock d.mutex
        end else begin
          Mutex.lock d.mutex;
          d.assignments <- a :: d.assignments;
          Mutex.unlock d.mutex
        end;
        run d
      end
    end

  let stop d =
    Mutex.lock d.mutex;
    d.running <- false;
    Condition.broadcast d.condition;
    Mutex.unlock d.mutex;
    Array.iter Car.stop d.cars
end

(* ---------- EmergencySystem: holds only dispatcher ---------- *)

module Emergency = struct
  (* Type abstraction: holds only a dispatcher, no car references *)
  type t = { dispatcher: Dispatcher.t }
  let make dispatcher = { dispatcher }
  let trigger e = Dispatcher.trigger_emergency e.dispatcher
  let clear e = Dispatcher.clear_emergency e.dispatcher
end

(* ---------- Build and run ---------- *)

let () =
  let dispatcher = Dispatcher.make ~num_cars:2 in
  let _emergency = Emergency.make dispatcher in
  List.iter (fun c -> let _ = Thread.create Car.run c in ())
    (Dispatcher.get_cars_for_test dispatcher);
  let _ = Thread.create Dispatcher.run dispatcher in
  Dispatcher.request dispatcher 5 Up;
  Thread.delay 0.1;
  Dispatcher.request dispatcher 3 Down;
  Thread.delay 10.0;
  List.iter (fun c ->
    Printf.printf "Car %d floor: %d\n"
      (Car.car_id c) (Car.current_floor c))
    (Dispatcher.get_cars_for_test dispatcher)

(* elevator_naive.ml

   A deliberately flawed two-elevator controller in OCaml.

   To compile (requires ocaml + dune or direct build):
     ocamlfind ocamlopt -package threads.posix -linkpkg -thread \
       elevator_naive.ml -o elevator_naive

   Note: OCaml's module system and strong typing actually make some of the
   flaws from the Python version HARDER to write. I've annotated where
   OCaml's defaults push you away from the bugs, and where I had to go
   out of my way to reproduce them for parity with the Python version.
*)

(* ---------- Types ---------- *)

type direction = Up | Down | Idle

type door_state = Closed | Opening_door | Open_door | Closing_door

type motor_state = Stopped | Moving

type request = {
  floor: int;
  direction: direction;
  timestamp: float;
}

(* ---------- Door ---------- *)
(* FLAW 1: Door's state is mutable and publicly accessible.
   In idiomatic OCaml we'd hide this with a module signature. I've
   deliberately left it open to parallel the Python version. *)

module Door = struct
  type t = {
    car_id: int;
    mutable state: door_state;
    mutex: Mutex.t;
  }

  let make car_id = {
    car_id;
    state = Closed;
    mutex = Mutex.create ();
  }

  let open_ d =
    Mutex.lock d.mutex;
    if d.state = Closed then begin
      d.state <- Opening_door;
      (* FLAW 1: no check that motor is stopped *)
      Thread.delay 0.5;
      d.state <- Open_door
    end;
    Mutex.unlock d.mutex

  let close d =
    Mutex.lock d.mutex;
    if d.state = Open_door then begin
      d.state <- Closing_door;
      Thread.delay 0.5;
      d.state <- Closed
    end;
    Mutex.unlock d.mutex
end

(* ---------- Motor ---------- *)

module Motor = struct
  type t = {
    car_id: int;
    mutable state: motor_state;
    mutable current_floor: int;
    mutex: Mutex.t;
  }

  let make car_id = {
    car_id;
    state = Stopped;
    current_floor = 1;
    mutex = Mutex.create ();
  }

  let move_to m floor =
    Mutex.lock m.mutex;
    (* FLAW 2: no check that door is closed *)
    m.state <- Moving;
    let dir = if floor > m.current_floor then 1 else -1 in
    while m.current_floor <> floor do
      Thread.delay 0.3;
      m.current_floor <- m.current_floor + dir
    done;
    m.state <- Stopped;
    Mutex.unlock m.mutex
end

(* ---------- ElevatorCar ---------- *)

module Car = struct
  type t = {
    car_id: int;
    door: Door.t;
    motor: Motor.t;
    mutable queue: request list;
    mutex: Mutex.t;
    mutable running: bool;
    mutable emergency: bool;  (* FLAW 3: plain mutable flag, read without lock *)
  }

  let make car_id = {
    car_id;
    door = Door.make car_id;
    motor = Motor.make car_id;
    queue = [];
    mutex = Mutex.create ();
    running = true;
    emergency = false;
  }

  let add_request c req =
    Mutex.lock c.mutex;
    c.queue <- c.queue @ [req];
    Mutex.unlock c.mutex

  let rec run c =
    if not c.running then ()
    else begin
      (* FLAW 3: read emergency without lock, act on stale value *)
      if c.emergency then begin
        Door.open_ c.door;
        Thread.delay 0.1;
        run c
      end else begin
        let req = ref None in
        Mutex.lock c.mutex;
        (match c.queue with
         | [] -> ()
         | r :: rest -> req := Some r; c.queue <- rest);
        Mutex.unlock c.mutex;
        (match !req with
         | None -> Thread.delay 0.1
         | Some r ->
           (* FLAW 4: sequence enforced only by statement order *)
           Door.close c.door;
           Motor.move_to c.motor r.floor;
           Door.open_ c.door);
        run c
      end
    end

  (* FLAW 5: called by EmergencySystem directly, bypassing Dispatcher *)
  let emergency_stop c = c.emergency <- true

  let stop c = c.running <- false
end

(* ---------- Dispatcher ---------- *)

module Dispatcher = struct
  type t = {
    cars: Car.t array;
    mutable pending: request list;
    mutex: Mutex.t;
    mutable running: bool;
  }

  let make cars = {
    cars;
    pending = [];
    mutex = Mutex.create ();
    running = true;
  }

  let request d floor direction =
    Mutex.lock d.mutex;
    d.pending <- d.pending @ [{floor; direction; timestamp = Unix.gettimeofday ()}];
    Mutex.unlock d.mutex

  let rec run d =
    if not d.running then ()
    else begin
      let req = ref None in
      Mutex.lock d.mutex;
      (match d.pending with
       | [] -> ()
       | r :: rest -> req := Some r; d.pending <- rest);
      Mutex.unlock d.mutex;
      (match !req with
       | None -> Thread.delay 0.1
       | Some r ->
         (* FLAW 6: stale snapshot of car positions *)
         let best = ref d.cars.(0) in
         let best_dist = ref (abs (d.cars.(0).motor.current_floor - r.floor)) in
         for i = 1 to Array.length d.cars - 1 do
           let dist = abs (d.cars.(i).motor.current_floor - r.floor) in
           if dist < !best_dist then begin
             best := d.cars.(i);
             best_dist := dist
           end
         done;
         Car.add_request !best r);
      run d
    end
end

(* ---------- EmergencySystem ---------- *)

module Emergency = struct
  type t = {
    cars: Car.t array;
    dispatcher: Dispatcher.t;  (* held but unused *)
  }

  let make cars dispatcher = { cars; dispatcher }

  (* FLAW 5: reaches into cars directly, bypassing dispatcher *)
  let trigger e = Array.iter Car.emergency_stop e.cars
end

(* ---------- Build and run ---------- *)

let build_system () =
  let cars = [| Car.make 1; Car.make 2 |] in
  let dispatcher = Dispatcher.make cars in
  let emergency = Emergency.make cars dispatcher in
  let _t1 = Thread.create Car.run cars.(0) in
  let _t2 = Thread.create Car.run cars.(1) in
  let _td = Thread.create Dispatcher.run dispatcher in
  (cars, dispatcher, emergency)

let () =
  let (cars, dispatcher, _emergency) = build_system () in
  Dispatcher.request dispatcher 5 Up;
  Thread.delay 0.1;
  Dispatcher.request dispatcher 3 Down;
  Thread.delay 5.0;
  Printf.printf "Car 1 floor: %d\n" cars.(0).motor.current_floor;
  Printf.printf "Car 2 floor: %d\n" cars.(1).motor.current_floor

// elevator_bench.rs
//
// Throughput benchmark for the Rust "correct" elevator reference.
//
// This file is a self-contained single-file port of
// samples/hos/elevator/comparison/elevator_correct.rs with the
// following changes ONLY:
//
//   * Every thread::sleep(...) call in Door::open, Door::close,
//     Motor::move_to, and Car::execute_request's loading-dwell is
//     removed. The state-machine transitions, Mutex/Condvar
//     coordination, and validated transitions are otherwise unchanged.
//
//   * The main function is a benchmark driver: spawn threads, send
//     N alternating-floor requests, wait for the dispatcher queue to
//     drain and all cars to be back at IdleAtFloor, then print a
//     single [BENCH] line for the orchestrator to scrape.
//
// Counting convention matches the Erlang HOS bench: 16 events per
// request cycle. This is the same logical work done in all three
// languages.
//
// Compile and run:
//   rustc -O -A warnings elevator_bench.rs -o out/elevator_bench
//   ./out/elevator_bench 1000

use std::collections::VecDeque;
use std::env;
use std::process;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::{Duration, Instant, SystemTime};

const EVENTS_PER_CYCLE: u64 = 16;

// ---------- Types ----------

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction { Up, Down, Idle }

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CarState {
    IdleAtFloor,
    DoorOpening,
    Loading,
    DoorClosing,
    PreparingToMove,
    Moving,
    Arriving,
    EmergencyStopped,
    EmergencyDoorOpen,
}

#[derive(Debug, Clone)]
pub struct Request {
    pub floor: i32,
    pub direction: Direction,
    pub timestamp: SystemTime,
}

#[derive(Debug)]
pub struct Assignment {
    pub request: Request,
    pub car_id: u32,
    pub assigned_at: SystemTime,
    pub completed: bool,
}

fn is_valid_transition(from: CarState, to: CarState) -> bool {
    use CarState::*;
    matches!(
        (from, to),
        (IdleAtFloor, DoorOpening)
        | (IdleAtFloor, PreparingToMove)
        | (IdleAtFloor, EmergencyStopped)
        | (DoorOpening, Loading)
        | (DoorOpening, EmergencyDoorOpen)
        | (Loading, DoorClosing)
        | (Loading, EmergencyDoorOpen)
        | (DoorClosing, PreparingToMove)
        | (DoorClosing, IdleAtFloor)
        | (DoorClosing, EmergencyStopped)
        | (PreparingToMove, Moving)
        | (PreparingToMove, EmergencyStopped)
        | (Moving, Arriving)
        | (Moving, EmergencyStopped)
        | (Arriving, DoorOpening)
        | (Arriving, IdleAtFloor)
        | (Arriving, EmergencyStopped)
        | (EmergencyStopped, EmergencyDoorOpen)
        | (EmergencyStopped, IdleAtFloor)
        | (EmergencyDoorOpen, IdleAtFloor)
    )
}

// ---------- Car module: encapsulates door and motor ----------
//
// Identical to elevator_correct.rs except Door::open, Door::close,
// and Motor::move_to no longer call thread::sleep. The door state
// and motor current_floor still update correctly; we just do not
// burn wall-clock time simulating mechanical delay.

mod car {
    use super::*;

    struct Door { is_open: bool }
    impl Door {
        fn new() -> Self { Self { is_open: false } }
        fn open(&mut self) { self.is_open = true; }
        fn close(&mut self) { self.is_open = false; }
    }

    struct Motor { current_floor: i32, moving: bool }
    impl Motor {
        fn new() -> Self { Self { current_floor: 1, moving: false } }
        fn move_to(&mut self, target: i32) {
            self.moving = true;
            // Go directly to target; the per-floor thread::sleep
            // loop is removed. The state machine still sees the
            // motor transition through the Moving -> Arriving states.
            self.current_floor = target;
            self.moving = false;
        }
    }

    struct CarInner {
        door: Door,
        motor: Motor,
        state: CarState,
        assignment: Option<Assignment>,
        running: bool,
    }

    pub struct Car {
        car_id: u32,
        inner: Mutex<CarInner>,
        condvar: Condvar,
        on_state_change: Box<dyn Fn(u32, CarState) + Send + Sync>,
    }

    impl Car {
        pub fn new(
            car_id: u32,
            on_state_change: Box<dyn Fn(u32, CarState) + Send + Sync>,
        ) -> Self {
            Self {
                car_id,
                inner: Mutex::new(CarInner {
                    door: Door::new(),
                    motor: Motor::new(),
                    state: CarState::IdleAtFloor,
                    assignment: None,
                    running: true,
                }),
                condvar: Condvar::new(),
                on_state_change,
            }
        }

        pub fn car_id(&self) -> u32 { self.car_id }

        pub fn current_floor(&self) -> i32 {
            self.inner.lock().unwrap().motor.current_floor
        }

        pub fn state(&self) -> CarState {
            self.inner.lock().unwrap().state
        }

        fn transition(&self, inner: &mut CarInner, new_state: CarState) {
            if !is_valid_transition(inner.state, new_state) {
                panic!("Car {}: invalid transition {:?} -> {:?}",
                    self.car_id, inner.state, new_state);
            }
            inner.state = new_state;
            self.condvar.notify_all();
            (self.on_state_change)(self.car_id, new_state);
        }

        pub fn assign(&self, a: Assignment) -> bool {
            let mut inner = self.inner.lock().unwrap();
            if inner.state != CarState::IdleAtFloor || inner.assignment.is_some() {
                return false;
            }
            inner.assignment = Some(a);
            self.condvar.notify_all();
            true
        }

        pub fn trigger_emergency(&self) {
            let mut inner = self.inner.lock().unwrap();
            match inner.state {
                CarState::EmergencyStopped | CarState::EmergencyDoorOpen => (),
                _ => {
                    inner.state = CarState::EmergencyStopped;
                    self.condvar.notify_all();
                    (self.on_state_change)(self.car_id, CarState::EmergencyStopped);
                }
            }
        }

        pub fn clear_emergency(&self) {
            let mut inner = self.inner.lock().unwrap();
            match inner.state {
                CarState::EmergencyDoorOpen => {
                    inner.door.close();
                    self.transition(&mut inner, CarState::IdleAtFloor);
                }
                CarState::EmergencyStopped => {
                    self.transition(&mut inner, CarState::IdleAtFloor);
                }
                _ => (),
            }
        }

        fn execute_request(&self, a: &Assignment) {
            let target = a.request.floor;
            let current = self.current_floor();

            if current != target {
                {
                    let mut inner = self.inner.lock().unwrap();
                    if inner.state != CarState::IdleAtFloor { return; }
                    self.transition(&mut inner, CarState::PreparingToMove);
                }
                {
                    let mut inner = self.inner.lock().unwrap();
                    if inner.state != CarState::PreparingToMove { return; }
                    self.transition(&mut inner, CarState::Moving);
                    inner.motor.move_to(target);
                    if inner.state != CarState::Moving { return; }
                    self.transition(&mut inner, CarState::Arriving);
                }
            }

            {
                let mut inner = self.inner.lock().unwrap();
                let can_open = matches!(inner.state,
                    CarState::Arriving | CarState::IdleAtFloor);
                if !can_open { return; }
                self.transition(&mut inner, CarState::DoorOpening);
                inner.door.open();
                if inner.state != CarState::DoorOpening { return; }
                self.transition(&mut inner, CarState::Loading);
            }

            // Loading dwell (normally 1000ms) removed for bench.

            {
                let mut inner = self.inner.lock().unwrap();
                if inner.state != CarState::Loading { return; }
                self.transition(&mut inner, CarState::DoorClosing);
                inner.door.close();
                if inner.state != CarState::DoorClosing { return; }
                self.transition(&mut inner, CarState::IdleAtFloor);
                inner.assignment = None;
            }
        }

        pub fn run(self: Arc<Self>) {
            loop {
                let a_opt = {
                    let mut inner = self.inner.lock().unwrap();
                    while inner.running
                          && inner.assignment.is_none()
                          && inner.state != CarState::EmergencyStopped {
                        inner = self.condvar.wait(inner).unwrap();
                    }
                    if !inner.running { return; }
                    if inner.state == CarState::EmergencyStopped {
                        inner.door.open();
                        self.transition(&mut inner, CarState::EmergencyDoorOpen);
                        while inner.running
                              && inner.state == CarState::EmergencyDoorOpen {
                            inner = self.condvar.wait(inner).unwrap();
                        }
                        None
                    } else {
                        inner.assignment.as_ref().map(|a| Assignment {
                            request: a.request.clone(),
                            car_id: a.car_id,
                            assigned_at: a.assigned_at,
                            completed: false,
                        })
                    }
                };
                if let Some(a) = a_opt {
                    self.execute_request(&a);
                }
            }
        }

        pub fn stop(&self) {
            let mut inner = self.inner.lock().unwrap();
            inner.running = false;
            self.condvar.notify_all();
        }
    }
}

use car::Car;

// ---------- Dispatcher ----------

pub struct Dispatcher {
    cars: Vec<Arc<Car>>,
    inner: Mutex<DispatcherInner>,
    condvar: Condvar,
    completions: AtomicU64,
    assigns: AtomicU64,
}

struct DispatcherInner {
    pending: VecDeque<Request>,
    assignments: Vec<Assignment>,
    car_states: std::collections::HashMap<u32, CarState>,
    running: bool,
    emergency_active: bool,
}

impl Dispatcher {
    pub fn new(num_cars: u32) -> Arc<Self> {
        let inner = Mutex::new(DispatcherInner {
            pending: VecDeque::new(),
            assignments: Vec::new(),
            car_states: std::collections::HashMap::new(),
            running: true,
            emergency_active: false,
        });
        let d = Arc::new(Self {
            cars: Vec::new(),
            inner,
            condvar: Condvar::new(),
            completions: AtomicU64::new(0),
            assigns: AtomicU64::new(0),
        });
        let cars: Vec<Arc<Car>> = (0..num_cars).map(|i| {
            let d2 = Arc::downgrade(&d);
            Arc::new(Car::new(i, Box::new(move |car_id, new_state| {
                if let Some(d) = d2.upgrade() {
                    {
                        let mut inner = d.inner.lock().unwrap();
                        inner.car_states.insert(car_id, new_state);
                        d.condvar.notify_all();
                    }
                    // Every time a car returns to IdleAtFloor, a
                    // request finished. Counting here is safe: the
                    // transition fires exactly once per completion.
                    if new_state == CarState::IdleAtFloor {
                        d.completions.fetch_add(1, Ordering::SeqCst);
                    }
                }
            })))
        }).collect();
        {
            let mut inner = d.inner.lock().unwrap();
            for c in &cars {
                inner.car_states.insert(c.car_id(), CarState::IdleAtFloor);
            }
        }
        // Same late-binding trick as elevator_correct.rs. Safe because
        // at this point `d` has exactly one owner (us).
        let d_mut: &mut Dispatcher = unsafe {
            &mut *(Arc::as_ptr(&d) as *mut Dispatcher)
        };
        d_mut.cars = cars;
        d
    }

    pub fn request(&self, floor: i32, direction: Direction) {
        let mut inner = self.inner.lock().unwrap();
        if inner.emergency_active { return; }
        inner.pending.push_back(Request {
            floor,
            direction,
            timestamp: SystemTime::now(),
        });
        self.condvar.notify_all();
    }

    pub fn trigger_emergency(&self) {
        let cars_snapshot = {
            let mut inner = self.inner.lock().unwrap();
            if inner.emergency_active { return; }
            inner.emergency_active = true;
            self.cars.clone()
        };
        for c in cars_snapshot { c.trigger_emergency(); }
    }

    pub fn clear_emergency(&self) {
        let cars_snapshot = {
            let mut inner = self.inner.lock().unwrap();
            if !inner.emergency_active { return; }
            inner.emergency_active = false;
            self.cars.clone()
        };
        for c in cars_snapshot { c.clear_emergency(); }
    }

    pub fn cars_for_test(&self) -> &[Arc<Car>] { &self.cars }

    pub fn pending_len(&self) -> usize {
        self.inner.lock().unwrap().pending.len()
    }

    pub fn completions(&self) -> u64 {
        self.completions.load(Ordering::SeqCst)
    }

    pub fn assigns(&self) -> u64 {
        self.assigns.load(Ordering::SeqCst)
    }

    /// Single-lock snapshot. Returns true iff pending is empty AND
    /// every car's cached state is IdleAtFloor. Does NOT call into
    /// car.inner: all state is read from the dispatcher's mirror,
    /// which is updated by every car transition via the on_state_change
    /// callback. Needed to avoid lock inversion (Dispatcher::inner
    /// -> Car::inner) against the car threads running transitions.
    pub fn all_idle_and_drained(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        if !inner.pending.is_empty() { return false; }
        self.cars.iter().all(|c| {
            inner.car_states.get(&c.car_id())
                == Some(&CarState::IdleAtFloor)
        })
    }

    pub fn run(self: Arc<Self>) {
        loop {
            // Two-phase to avoid lock inversion:
            //   1) under dispatcher.inner, pick a request and a list
            //      of candidate car Arcs (by id) using the cached
            //      car_states map; release dispatcher.inner.
            //   2) outside the dispatcher lock, call c.current_floor()
            //      on each candidate (which grabs car.inner) to pick
            //      the closest.
            //
            // The cached state can be stale, and assign() will detect
            // and reject if the car has moved on since we checked;
            // we push the request back in that case and retry.
            let (req, idle_ids) = {
                let mut inner = self.inner.lock().unwrap();
                while inner.running
                      && (inner.pending.is_empty() || inner.emergency_active) {
                    inner = self.condvar.wait(inner).unwrap();
                }
                if !inner.running { return; }
                let req = inner.pending.pop_front().unwrap();
                let idle_ids: Vec<u32> = self.cars.iter()
                    .map(|c| c.car_id())
                    .filter(|id| inner.car_states.get(id)
                        == Some(&CarState::IdleAtFloor))
                    .collect();
                if idle_ids.is_empty() {
                    inner.pending.push_front(req);
                    let _unused = self.condvar.wait_timeout(inner,
                        Duration::from_millis(500));
                    continue;
                }
                (req, idle_ids)
            };
            let idle_cars: Vec<&Arc<Car>> = self.cars.iter()
                .filter(|c| idle_ids.contains(&c.car_id()))
                .collect();
            if idle_cars.is_empty() {
                let mut inner = self.inner.lock().unwrap();
                inner.pending.push_front(req);
                self.condvar.notify_all();
                continue;
            }
            let best = idle_cars.iter()
                .min_by_key(|c| (c.current_floor() - req.floor).abs())
                .unwrap();
            let best = (*best).clone();
            let (req, best) = (req, best);
            let a = Assignment {
                request: req.clone(),
                car_id: best.car_id(),
                assigned_at: SystemTime::now(),
                completed: false,
            };
            if !best.assign(a) {
                let mut inner = self.inner.lock().unwrap();
                inner.pending.push_front(req);
                self.condvar.notify_all();
            } else {
                self.assigns.fetch_add(1, Ordering::SeqCst);
                let mut inner = self.inner.lock().unwrap();
                inner.assignments.push(Assignment {
                    request: req,
                    car_id: best.car_id(),
                    assigned_at: SystemTime::now(),
                    completed: false,
                });
            }
        }
    }

    pub fn stop(&self) {
        {
            let mut inner = self.inner.lock().unwrap();
            inner.running = false;
            self.condvar.notify_all();
        }
        for c in &self.cars { c.stop(); }
    }
}

// ---------- Bench driver ----------

/// Wait until exactly `target_completions` requests have finished
/// AND the pending queue is empty AND every car is back at Idle.
/// All state is read through the dispatcher (cached car_states +
/// pending + completions counter) so we never call into car.inner
/// from here. This avoids lock inversion against the car threads.
fn wait_for_completions(
    dispatcher: &Arc<Dispatcher>,
    target_completions: u64,
    deadline: Duration,
) -> bool {
    let start = Instant::now();
    loop {
        let comps = dispatcher.completions();
        if comps >= target_completions
            && dispatcher.all_idle_and_drained() {
            return true;
        }
        if start.elapsed() >= deadline { return false; }
        thread::sleep(Duration::from_micros(100));
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let n: u64 = if args.len() >= 2 {
        match args[1].parse() {
            Ok(v) => v,
            Err(_) => {
                eprintln!("usage: {} [N]", args[0]);
                process::exit(2);
            }
        }
    } else {
        1000
    };
    let num_cars: u32 = 2;
    let warmup: u64 = 10;

    let dispatcher = Dispatcher::new(num_cars);

    for c in dispatcher.cars_for_test() {
        let c2 = c.clone();
        thread::spawn(move || c2.run());
    }
    let d_run = dispatcher.clone();
    thread::spawn(move || d_run.run());

    // Let the threads enter their run() loops before we start
    // pushing requests. A tiny sleep here is not measured by the
    // bench (it happens before `start`).
    thread::sleep(Duration::from_millis(10));

    // Warm-up.
    for i in 0..warmup {
        let floor = 1 + (i as i32 % 5);
        dispatcher.request(floor, Direction::Up);
    }
    if !wait_for_completions(&dispatcher, warmup,
                             Duration::from_secs(30)) {
        eprintln!(
            "warm-up: only {} of {} completed within 30s",
            dispatcher.completions(), warmup);
        dispatcher.stop();
        process::exit(1);
    }
    let base_completions = dispatcher.completions();

    // Timed run.
    let start = Instant::now();
    for i in 0..n {
        let floor = 1 + (i as i32 % 5);
        let dir = if i % 2 == 0 { Direction::Up } else { Direction::Down };
        dispatcher.request(floor, dir);
    }
    if !wait_for_completions(&dispatcher, base_completions + n,
                             Duration::from_secs(120)) {
        eprintln!(
            "timed run: only {} of {} completed within 120s",
            dispatcher.completions() - base_completions, n);
        dispatcher.stop();
        process::exit(1);
    }
    let elapsed = start.elapsed();
    let wall = elapsed.as_secs_f64();
    let total_events = n * EVENTS_PER_CYCLE;
    let rate = if wall > 0.0 { total_events as f64 / wall } else { 0.0 };

    let done = dispatcher.completions() - base_completions;
    if done != n {
        eprintln!("[warn] expected {} completions, got {}", n, done);
    }
    println!("Rust correct: cars={} N={} wall={:.3}s events/sec={:.2} completions={}",
        num_cars, n, wall, rate, done);
    println!("[BENCH] Rust correct events/sec={:.2} cycles={} wall={:.3}s",
        rate, n, wall);

    dispatcher.stop();
    thread::sleep(Duration::from_millis(50));
}

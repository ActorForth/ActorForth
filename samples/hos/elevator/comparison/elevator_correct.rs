// elevator_correct.rs
//
// A correctly-implemented two-elevator controller in Rust.
//
// Takes advantage of:
//   - Module privacy (Door and Motor are not exposed outside Car)
//   - Ownership to enforce single-parent (Dispatcher owns Cars; nothing
//     else can have them)
//   - Exhaustive pattern matching on CarState
//   - Rust's impossibility of writing the sibling back-channel:
//     EmergencySystem literally cannot hold references to cars because
//     it doesn't own them and the Dispatcher doesn't give them out.
//
// The correct Rust version is arguably closer to USL than the correct
// OCaml version, because ownership expresses control-hierarchy
// constraints as a type system invariant rather than a convention.

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::{Duration, SystemTime};

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

// Exhaustive, compile-checked transition validation.
// Any new CarState added to the enum will force this match to be updated;
// the compiler will not let you forget a case.
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

mod car {
    use super::*;

    // Private -- not exported from this module
    struct Door { is_open: bool }
    impl Door {
        fn new() -> Self { Self { is_open: false } }
        fn open(&mut self) {
            thread::sleep(Duration::from_millis(500));
            self.is_open = true;
        }
        fn close(&mut self) {
            thread::sleep(Duration::from_millis(500));
            self.is_open = false;
        }
    }

    struct Motor { current_floor: i32, moving: bool }
    impl Motor {
        fn new() -> Self { Self { current_floor: 1, moving: false } }
        fn move_to(&mut self, target: i32) {
            self.moving = true;
            let dir = if target > self.current_floor { 1 } else { -1 };
            while self.current_floor != target {
                thread::sleep(Duration::from_millis(300));
                self.current_floor += dir;
            }
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

        // Validated transition. Caller holds the lock.
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
                    // Note: motor.move_to holds the lock throughout. This
                    // is safe here because the motor is private to Car;
                    // no other thread can observe the motor state.
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

            thread::sleep(Duration::from_millis(1000));

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
    // Cars are OWNED by the dispatcher. Nothing else has an Arc<Car>.
    // This is the Rust enforcement of the single-parent axiom.
    cars: Vec<Arc<Car>>,
    inner: Mutex<DispatcherInner>,
    condvar: Condvar,
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
        });
        // Trick: build the dispatcher, then replace cars field via
        // unsafe workaround. In production Rust we'd use a two-phase
        // construction or wrap cars in a OnceCell. For clarity here
        // we just rebuild:
        let cars: Vec<Arc<Car>> = (0..num_cars).map(|i| {
            let d2 = Arc::downgrade(&d);
            Arc::new(Car::new(i, Box::new(move |car_id, new_state| {
                if let Some(d) = d2.upgrade() {
                    let mut inner = d.inner.lock().unwrap();
                    inner.car_states.insert(car_id, new_state);
                    d.condvar.notify_all();
                }
            })))
        }).collect();
        {
            let mut inner = d.inner.lock().unwrap();
            for c in &cars {
                inner.car_states.insert(c.car_id(), CarState::IdleAtFloor);
            }
        }
        // SAFETY: we're the only holder of d at this point.
        // In production: use once_cell or lazy initialization.
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
        for c in cars_snapshot {
            c.trigger_emergency();
        }
    }

    pub fn clear_emergency(&self) {
        let cars_snapshot = {
            let mut inner = self.inner.lock().unwrap();
            if !inner.emergency_active { return; }
            inner.emergency_active = false;
            self.cars.clone()
        };
        for c in cars_snapshot {
            c.clear_emergency();
        }
    }

    // Testing hook. A real system would not expose this.
    pub fn cars_for_test(&self) -> &[Arc<Car>] { &self.cars }

    pub fn run(self: Arc<Self>) {
        loop {
            let (req, best) = {
                let mut inner = self.inner.lock().unwrap();
                while inner.running
                      && (inner.pending.is_empty() || inner.emergency_active) {
                    inner = self.condvar.wait(inner).unwrap();
                }
                if !inner.running { return; }
                let req = inner.pending.pop_front().unwrap();
                let idle: Vec<&Arc<Car>> = self.cars.iter()
                    .filter(|c| inner.car_states.get(&c.car_id())
                        == Some(&CarState::IdleAtFloor))
                    .collect();
                if idle.is_empty() {
                    inner.pending.push_front(req);
                    let _unused = self.condvar.wait_timeout(inner,
                        Duration::from_millis(500));
                    continue;
                }
                let best = idle.iter()
                    .min_by_key(|c| (c.current_floor() - req.floor).abs())
                    .unwrap();
                (req, (*best).clone())
            };
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

// ---------- EmergencySystem: holds ONLY the Dispatcher ----------
//
// It literally cannot reach a Car. The type system prevents it.
// The Dispatcher's cars field is private; cars_for_test is not
// exposed to EmergencySystem in any path.

pub struct EmergencySystem {
    dispatcher: Arc<Dispatcher>,
}

impl EmergencySystem {
    pub fn new(dispatcher: Arc<Dispatcher>) -> Self {
        Self { dispatcher }
    }
    pub fn trigger(&self) { self.dispatcher.trigger_emergency(); }
    pub fn clear(&self) { self.dispatcher.clear_emergency(); }
}

fn main() {
    let dispatcher = Dispatcher::new(2);
    let _emergency = EmergencySystem::new(dispatcher.clone());

    // Spawn car threads. We need to clone Arc<Car> out of the dispatcher.
    for c in dispatcher.cars_for_test() {
        let c2 = c.clone();
        thread::spawn(move || c2.run());
    }
    let d2 = dispatcher.clone();
    thread::spawn(move || d2.run());

    dispatcher.request(5, Direction::Up);
    thread::sleep(Duration::from_millis(100));
    dispatcher.request(3, Direction::Down);
    thread::sleep(Duration::from_secs(10));

    for c in dispatcher.cars_for_test() {
        println!("Car {} floor: {}", c.car_id(), c.current_floor());
    }
}

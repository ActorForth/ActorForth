// elevator_naive.rs
//
// A deliberately flawed two-elevator controller in Rust.
//
// IMPORTANT: Writing this was genuinely hard. The borrow checker refuses
// several of the most natural "bad" patterns from the Python version.
// Specifically, the sibling back-channel in Bug 5 (EmergencySystem holding
// Vec<Arc<ElevatorCar>>) requires Arc<Mutex<...>> everywhere, because
// without it you cannot share mutable references across threads at all.
// So even the "broken" version pays a correctness tax that Python doesn't.
//
// This tells you something: Rust makes it HARDER to write the broken version
// and arguably impossible to write the Python-style broken version verbatim.
// Many of the flaws are attenuated even in the naive version.
//
// Build: cargo new elevator_naive && cargo add --features none
// Then put this as src/main.rs and run: cargo run

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Direction { Up, Down, Idle }

#[derive(Debug, Clone, Copy, PartialEq)]
enum DoorState { Closed, Opening, Open, Closing }

#[derive(Debug, Clone, Copy, PartialEq)]
enum MotorState { Stopped, Moving }

#[derive(Debug, Clone)]
struct Request {
    floor: i32,
    #[allow(dead_code)]
    direction: Direction,
    #[allow(dead_code)]
    timestamp: SystemTime,
}

struct Door {
    #[allow(dead_code)]
    car_id: u32,
    state: DoorState,
}

impl Door {
    fn new(car_id: u32) -> Self { Self { car_id, state: DoorState::Closed } }

    fn open(&mut self) {
        if self.state == DoorState::Closed {
            self.state = DoorState::Opening;
            // FLAW 1: no check that motor is stopped
            thread::sleep(Duration::from_millis(500));
            self.state = DoorState::Open;
        }
    }

    fn close(&mut self) {
        if self.state == DoorState::Open {
            self.state = DoorState::Closing;
            thread::sleep(Duration::from_millis(500));
            self.state = DoorState::Closed;
        }
    }
}

struct Motor {
    #[allow(dead_code)]
    car_id: u32,
    state: MotorState,
    current_floor: i32,
}

impl Motor {
    fn new(car_id: u32) -> Self {
        Self { car_id, state: MotorState::Stopped, current_floor: 1 }
    }

    fn move_to(&mut self, floor: i32) {
        // FLAW 2: no check that door is closed
        self.state = MotorState::Moving;
        let dir = if floor > self.current_floor { 1 } else { -1 };
        while self.current_floor != floor {
            thread::sleep(Duration::from_millis(300));
            self.current_floor += dir;
        }
        self.state = MotorState::Stopped;
    }
}

struct ElevatorCar {
    car_id: u32,
    door: Mutex<Door>,
    motor: Mutex<Motor>,
    queue: Mutex<VecDeque<Request>>,
    running: Mutex<bool>,
    emergency: Mutex<bool>,  // FLAW 3: would be AtomicBool in real Rust
}

impl ElevatorCar {
    fn new(car_id: u32) -> Self {
        Self {
            car_id,
            door: Mutex::new(Door::new(car_id)),
            motor: Mutex::new(Motor::new(car_id)),
            queue: Mutex::new(VecDeque::new()),
            running: Mutex::new(true),
            emergency: Mutex::new(false),
        }
    }

    fn add_request(&self, req: Request) {
        self.queue.lock().unwrap().push_back(req);
    }

    fn current_floor(&self) -> i32 {
        self.motor.lock().unwrap().current_floor
    }

    fn run(self: Arc<Self>) {
        loop {
            if !*self.running.lock().unwrap() { break; }

            // FLAW 3: TOCTOU - read emergency, release lock, then act
            let is_emergency = *self.emergency.lock().unwrap();
            if is_emergency {
                self.door.lock().unwrap().open();
                thread::sleep(Duration::from_millis(100));
                continue;
            }

            let req = self.queue.lock().unwrap().pop_front();
            match req {
                None => thread::sleep(Duration::from_millis(100)),
                Some(r) => {
                    // FLAW 4: sequence by statement order, not structure.
                    // The borrow checker ensures the locks are released
                    // between calls, but it does NOT enforce the ordering
                    // of operations across lock acquisitions.
                    self.door.lock().unwrap().close();
                    self.motor.lock().unwrap().move_to(r.floor);
                    self.door.lock().unwrap().open();
                }
            }
        }
    }

    // FLAW 5: called by EmergencySystem directly. Still possible in Rust
    // because we gave out Arc<ElevatorCar>.
    fn emergency_stop(&self) {
        *self.emergency.lock().unwrap() = true;
    }

    #[allow(dead_code)]
    fn stop(&self) {
        *self.running.lock().unwrap() = false;
    }
}

struct Dispatcher {
    cars: Vec<Arc<ElevatorCar>>,
    pending: Mutex<VecDeque<Request>>,
    running: Mutex<bool>,
}

impl Dispatcher {
    fn new(cars: Vec<Arc<ElevatorCar>>) -> Self {
        Self {
            cars,
            pending: Mutex::new(VecDeque::new()),
            running: Mutex::new(true),
        }
    }

    fn request(&self, floor: i32, direction: Direction) {
        self.pending.lock().unwrap().push_back(Request {
            floor,
            direction,
            timestamp: SystemTime::now(),
        });
    }

    fn run(self: Arc<Self>) {
        loop {
            if !*self.running.lock().unwrap() { break; }

            let req = self.pending.lock().unwrap().pop_front();
            match req {
                None => thread::sleep(Duration::from_millis(100)),
                Some(r) => {
                    // FLAW 6: stale snapshot of floors
                    let best = self.cars.iter()
                        .min_by_key(|c| (c.current_floor() - r.floor).abs())
                        .unwrap();
                    best.add_request(r);
                }
            }
        }
    }
}

struct EmergencySystem {
    cars: Vec<Arc<ElevatorCar>>,
    #[allow(dead_code)]
    dispatcher: Arc<Dispatcher>,  // held but unused
}

impl EmergencySystem {
    fn new(cars: Vec<Arc<ElevatorCar>>, dispatcher: Arc<Dispatcher>) -> Self {
        Self { cars, dispatcher }
    }

    // FLAW 5: reaches into cars directly
    fn trigger(&self) {
        for c in &self.cars {
            c.emergency_stop();
        }
    }
}

fn main() {
    let cars: Vec<Arc<ElevatorCar>> = (1..=2)
        .map(|i| Arc::new(ElevatorCar::new(i)))
        .collect();

    let dispatcher = Arc::new(Dispatcher::new(cars.clone()));
    let _emergency = EmergencySystem::new(cars.clone(), dispatcher.clone());

    for c in &cars {
        let c2 = c.clone();
        thread::spawn(move || c2.run());
    }
    let d2 = dispatcher.clone();
    thread::spawn(move || d2.run());

    dispatcher.request(5, Direction::Up);
    thread::sleep(Duration::from_millis(100));
    dispatcher.request(3, Direction::Down);
    thread::sleep(Duration::from_secs(5));

    for c in &cars {
        println!("Car {} floor: {}", c.car_id, c.current_floor());
    }
}

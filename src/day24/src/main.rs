use z3::ast::*;
use z3::{Config, Context, SatResult, Solver};

const INPUT: &str = include_str!("../../../inputs/day24.txt");

fn parse_vec(s: &str) -> (i64, i64, i64) {
    let vec = s
        .split(',')
        .map(str::trim)
        .map(|n| n.parse().expect("Invalid number"))
        .collect::<Vec<_>>();

    if let &[x, y, z] = &vec[..] {
        (x, y, z)
    } else {
        panic!("Not 3 elements")
    }
}

fn main() {
    let mut config = Config::new();
    config.set_model_generation(true);
    config.set_proof_generation(false);

    let context = Context::new(&config);
    let solver = Solver::new(&context);

    let initial_x = Int::fresh_const(&context, "x");
    let initial_y = Int::fresh_const(&context, "y");
    let initial_z = Int::fresh_const(&context, "z");

    let velocity_x = Int::fresh_const(&context, "vx");
    let velocity_y = Int::fresh_const(&context, "vy");
    let velocity_z = Int::fresh_const(&context, "vz");

    let add_point = |pos: (i64, i64, i64), velocity: (i64, i64, i64)| {
        let intersect_time = Int::fresh_const(&context, "it");
        solver.assert(&intersect_time.ge(&Int::from_i64(&context, 0)));

        let x = Int::from_i64(&context, pos.0);
        let y = Int::from_i64(&context, pos.1);
        let z = Int::from_i64(&context, pos.2);

        let vx = Int::from_i64(&context, velocity.0);
        let vy = Int::from_i64(&context, velocity.1);
        let vz = Int::from_i64(&context, velocity.2);

        let six = &initial_x + &velocity_x * &intersect_time;
        let hix = x + vx * &intersect_time;
        solver.assert(&six._eq(&hix));

        let siy = &initial_y + &velocity_y * &intersect_time;
        let hiy = y + vy * &intersect_time;
        solver.assert(&siy._eq(&hiy));

        let siz = &initial_z + &velocity_z * &intersect_time;
        let hiz = z + vz * &intersect_time;
        solver.assert(&siz._eq(&hiz));
    };

    for line in INPUT.split_terminator('\n') {
        let (position, velocitiy) = line.split_once('@').expect("Invalid line");

        add_point(parse_vec(position), parse_vec(velocitiy));
    }

    println!("Checking...");

    match solver.check() {
        SatResult::Sat => {
            println!("Satisfied!");

            let model = solver.get_model().expect("No model");

            let get = |val: Int| -> i64 {
                model
                    .eval(&val, true)
                    .and_then(|val| val.as_i64())
                    .expect("Missing value")
            };

            let initial_x = get(initial_x);
            let initial_y = get(initial_y);
            let initial_z = get(initial_z);

            let velocity_x = get(velocity_x);
            let velocity_y = get(velocity_y);
            let velocity_z = get(velocity_z);

            println!(
                "{initial_x}, {initial_y}, {initial_z} @ {velocity_x}, {velocity_y}, {velocity_z}"
            );
            println!("=> {}", initial_x + initial_y + initial_z);
        }
        SatResult::Unknown => {
            println!("Unknown");
        }
        SatResult::Unsat => {
            println!("Unsatisfied");
        }
    }
}

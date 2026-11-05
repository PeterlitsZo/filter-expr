//! Internal stage benchmarks, enabled only by `bench-internals`.

use std::{future::Future, hint::black_box, time::Instant};

use filter_expr::FilterExpr;

use crate::{
    FilterExprEvaler, Value, asm_codegen::AsmCodegen, ast_runner::AstRunner,
    bc_codegen::BytecodeCodegen, bc_runner::BytecodeRunner, simple_context,
};

const SAMPLES: usize = 20;

// Prepare inputs outside the clock, notably the owned assembly consumed by
// bytecode generation. Time batches to amortize Instant and executor overhead.
async fn measure<I, O, F: Future<Output = O>>(
    name: &str,
    iterations: usize,
    mut setup: impl FnMut() -> I,
    mut operation: impl FnMut(I) -> F,
) {
    for _ in 0..iterations {
        black_box(operation(black_box(setup())).await);
    }

    let mut samples = Vec::with_capacity(SAMPLES);
    for _ in 0..SAMPLES {
        let inputs: Vec<_> = (0..iterations).map(|_| setup()).collect();
        let start = Instant::now();
        for input in inputs {
            black_box(operation(black_box(input)).await);
        }
        samples.push(start.elapsed().as_secs_f64() * 1e9 / iterations as f64);
    }
    samples.sort_by(f64::total_cmp);
    let median = (samples[SAMPLES / 2 - 1] + samples[SAMPLES / 2]) / 2.0;
    println!(
        "{name:<26} {median:>12.1} {min:>12.1} {max:>12.1}",
        min = samples[0],
        max = samples[SAMPLES - 1],
    );
}

/// Run the benchmark suite inside an async runtime.
pub async fn run() {
    let iterations = match std::env::var("BENCH_ITERATIONS") {
        Ok(value) => value
            .parse::<usize>()
            .ok()
            .filter(|n| *n > 0)
            .expect("BENCH_ITERATIONS must be a positive integer"),
        Err(std::env::VarError::NotPresent) => 1024,
        Err(error) => panic!("invalid BENCH_ITERATIONS: {error}"),
    };
    let ctx = simple_context! { "name": "John", "age": 25, "active": true };
    let cases = [
        ("comparison", "age > 18"),
        ("logical", "name = 'John' AND age > 18 AND active = true"),
        ("arithmetic", "age * 2 + 10 >= 60 AND age IN [18, 25, 30]"),
        ("calls", "name.to_uppercase() = 'JOHN' AND abs(age) > 18"),
        ("short_circuit", "age < 0 AND name.to_uppercase() = 'JOHN'"),
    ];
    println!("{SAMPLES} samples, {iterations} operations/sample; times in ns/op");
    for (name, source) in cases {
        let filter = FilterExpr::parse(source).unwrap();
        let expr = filter.expr().unwrap();
        let asm = AsmCodegen::new().codegen(expr);
        let bytecode = BytecodeCodegen::new().codegen(asm.clone());
        let evaler = FilterExprEvaler::new();
        let env = &evaler.env;

        // Validate the fixture and prime the public evaluator's bytecode cache.
        let expected = name != "short_circuit";
        assert_eq!(evaler.eval(&filter, &ctx).await.unwrap(), expected);
        assert_eq!(
            evaler.eval_by_ast_runner(&filter, &ctx).await.unwrap(),
            expected
        );
        // SAFETY: bytecode was produced by the project's code generators.
        assert!(matches!(
            unsafe { BytecodeRunner::new(&bytecode, env.clone()).run(&ctx).await }.unwrap(),
            Value::Bool(actual) if actual == expected
        ));

        println!("\n{name}: {source}");
        println!(
            "{:<26} {:>12} {:>12} {:>12}",
            "stage", "median", "min", "max"
        );
        measure(
            "parse (lexer + parser)",
            iterations,
            || (),
            |()| async { FilterExpr::parse(black_box(source)).unwrap() },
        )
        .await;
        measure(
            "ast -> asm",
            iterations,
            || (),
            |()| async { AsmCodegen::new().codegen(black_box(expr)) },
        )
        .await;
        measure(
            "asm -> bytecode",
            iterations,
            || asm.clone(),
            |input| async { BytecodeCodegen::new().codegen(input) },
        )
        .await;
        measure(
            "ast execution",
            iterations,
            || (),
            |()| async {
                AstRunner::new(black_box(expr), env.clone())
                    .run(black_box(&ctx))
                    .await
                    .unwrap()
            },
        )
        .await;
        measure(
            "bytecode execution",
            iterations,
            || (),
            |()| async {
                // SAFETY: bytecode was produced by the project's code generators.
                unsafe {
                    BytecodeRunner::new(black_box(&bytecode), env.clone())
                        .run(black_box(&ctx))
                        .await
                }
                .unwrap()
            },
        )
        .await;
        measure(
            "eval (cache hit)",
            iterations,
            || (),
            |()| async {
                evaler
                    .eval(black_box(&filter), black_box(&ctx))
                    .await
                    .unwrap()
            },
        )
        .await;
        measure(
            "parse + compile + run",
            iterations,
            || (),
            |()| async {
                let filter = FilterExpr::parse(black_box(source)).unwrap();
                let asm = AsmCodegen::new().codegen(filter.expr().unwrap());
                let bytecode = BytecodeCodegen::new().codegen(asm);
                // SAFETY: bytecode was produced by the project's code generators.
                unsafe {
                    BytecodeRunner::new(&bytecode, env.clone())
                        .run(black_box(&ctx))
                        .await
                }
                .unwrap()
            },
        )
        .await;
    }
}

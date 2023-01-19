use std::fs;
use std::path::Path;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use libcst_native::tokenize;
use rustpython_parser::lexer;

fn criterion_benchmark(c: &mut Criterion) {
    let contents =
        fs::read_to_string(Path::new("resources/test/fixtures/pydocstyle/D.py")).unwrap();
    c.bench_function("LibCST", |b| {
        b.iter(|| {
            let tokens = tokenize(&contents).unwrap();
            for token in tokens {
                black_box(token);
            }
        });
    });
    c.bench_function("RustPython", |b| {
        b.iter(|| {
            let tokens = lexer::make_tokenizer(&contents);
            for token in tokens.flatten() {
                black_box(token);
            }
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

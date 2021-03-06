#[macro_use]
extern crate criterion;

use criterion::Criterion;
use math_types::BigUint;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("regular add", |b| b.iter(|| 1u8 + 2u8));
    c.bench_function("regular mul", |b| b.iter(|| 1u8 * 2u8));
    c.bench_function("biguint new", |b| b.iter(|| BigUint::new(1u8)));
    c.bench_function("biguint add small", |b| b.iter(|| BigUint::new(1u8) + 2u8));
    c.bench_function("biguint add big", |b| b.iter(|| BigUint::new(std::u64::MAX) + std::u64::MAX));
    c.bench_function("biguint mul small", |b| b.iter(|| BigUint::new(2u8) * BigUint::new(3u8)));
    c.bench_function("biguint mul big", |b| b.iter(|| BigUint::new(std::u64::MAX) * BigUint::new(std::u64::MAX)));
    c.bench_function("biguint div small", |b| b.iter(|| BigUint::new(4u8) / BigUint::new(2u8)));
    c.bench_function("biguint div big", |b| b.iter(|| BigUint::new(std::u64::MAX) / BigUint::new(std::u64::MAX / 2)));
    c.bench_function("biguint shl", |b| b.iter(|| BigUint::new(std::u64::MAX) << 8));
    c.bench_function("biguint shr", |b| b.iter(|| BigUint::new(std::u128::MAX) >> 8));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

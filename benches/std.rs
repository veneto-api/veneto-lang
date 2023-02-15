
use criterion::{ criterion_main, criterion_group, Criterion };
fn load_std(c: &mut Criterion) { 
    c.bench_function("load_std", |b| { 
        b.iter(|| { let _ = veneto::std::parse_std(); })
    });
}
criterion_group!(benches, load_std);
criterion_main!(benches); 
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::path::Path;

pub mod math;

pub fn read_line<P>(filename: P) -> io::Result<String>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let mut buf = String::new();

    BufReader::new(file).read_line(&mut buf)?;
    Ok(buf.trim_end().to_string())
}

pub fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(BufReader::new(file).lines())
}

pub fn powerset<T>(slice: &[T]) -> Vec<Vec<T>>
where
    T: Clone,
{
    (0..(1 << slice.len()))
        .map(|mask| {
            slice
                .iter()
                .enumerate()
                .filter(|&(pos, _)| (mask >> pos) & 1 == 1)
                .map(|(_, x)| x.clone())
                .collect()
        })
        .collect()
}

pub fn findall<P, A, T>(mut predicate: P, array: A) -> Vec<usize>
where
    P: FnMut(&T) -> bool,
    A: AsRef<[T]>,
{
    let arr = array.as_ref();
    arr.iter()
        .enumerate()
        .filter(|&(_, x)| predicate(x))
        .map(|(idx, _)| idx)
        .collect()
}

pub fn countmap<I>(it: I) -> HashMap<I::Item, usize>
where
    I: IntoIterator,
    I::Item: Eq + core::hash::Hash,
{
    let mut ret: HashMap<I::Item, usize> = HashMap::new();
    for elm in it {
        ret.entry(elm).and_modify(|cnt| *cnt += 1).or_insert(1);
    }
    ret
}

#[macro_export]
macro_rules! run_solver {
    ($x:literal) => {
        fn main() {
            use std::time::Instant;
            let now = Instant::now();
            let result = solve();
            let elapsed_time = now.elapsed();

            println!("[Problem {}]", $x);
            println!("Answer: {}", result);
            println!(
                "Elapsed time: {}.{:06} sec.",
                elapsed_time.as_secs(),
                elapsed_time.subsec_micros()
            );
        }
    };
}

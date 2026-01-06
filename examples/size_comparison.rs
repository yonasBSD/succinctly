//! Quick benchmark comparing succinctly vs serde_json at different file sizes

use std::time::Instant;
use succinctly::json::light::{JsonIndex, StandardJson};

fn main() {
    let sizes = [
        ("10mb", "data/bench/generated/comprehensive/10mb.json"),
        ("100mb", "data/bench/generated/comprehensive/100mb.json"),
        ("1gb", "data/bench/generated/comprehensive/1gb.json"),
    ];

    for (name, path) in sizes {
        let path = std::path::Path::new(path);
        if !path.exists() {
            eprintln!("Skipping {}: file not found", name);
            continue;
        }

        let bytes = std::fs::read(path).expect("Failed to read");
        let file_size_mb = bytes.len() as f64 / 1024.0 / 1024.0;

        println!("\n=== {} ({:.1} MB) ===", name, file_size_mb);

        // serde_json parse
        let start = Instant::now();
        let value: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        let serde_parse = start.elapsed();

        // serde_json traverse
        let start = Instant::now();
        let serde_count = count_serde(&value);
        let serde_traverse = start.elapsed();

        // succinctly index
        let start = Instant::now();
        let index = JsonIndex::build(&bytes);
        let succ_index = start.elapsed();

        // succinctly traverse
        let start = Instant::now();
        let root = index.root(&bytes);
        let succ_count = count_succinctly(root.value());
        let succ_traverse = start.elapsed();

        println!(
            "serde_json:  parse={:>7.1}ms  traverse={:>7.1}ms  total={:>7.1}ms  nodes={}",
            serde_parse.as_secs_f64() * 1000.0,
            serde_traverse.as_secs_f64() * 1000.0,
            (serde_parse + serde_traverse).as_secs_f64() * 1000.0,
            serde_count
        );
        println!(
            "succinctly:  index={:>7.1}ms  traverse={:>7.1}ms  total={:>7.1}ms  nodes={}",
            succ_index.as_secs_f64() * 1000.0,
            succ_traverse.as_secs_f64() * 1000.0,
            (succ_index + succ_traverse).as_secs_f64() * 1000.0,
            succ_count
        );

        let speedup = (serde_parse + serde_traverse).as_secs_f64()
            / (succ_index + succ_traverse).as_secs_f64();
        if speedup > 1.0 {
            println!("succinctly is {:.2}x faster", speedup);
        } else {
            println!("serde_json is {:.2}x faster", 1.0 / speedup);
        }
    }
}

fn count_serde(v: &serde_json::Value) -> usize {
    match v {
        serde_json::Value::Array(arr) => 1 + arr.iter().map(count_serde).sum::<usize>(),
        serde_json::Value::Object(obj) => 1 + obj.values().map(count_serde).sum::<usize>(),
        _ => 1,
    }
}

fn count_succinctly(v: StandardJson) -> usize {
    match v {
        StandardJson::Array(mut elements) => {
            let mut count = 1;
            while let Some(elem) = elements.next() {
                count += count_succinctly(elem);
            }
            count
        }
        StandardJson::Object(mut entries) => {
            let mut count = 1;
            while let Some(entry) = entries.next() {
                count += count_succinctly(entry.value());
            }
            count
        }
        _ => 1,
    }
}

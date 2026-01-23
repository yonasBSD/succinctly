//! Verify: open_idx = ib_idx + containers_before_this_ib

use succinctly::yaml::YamlIndex;

#[test]
fn verify_formula() {
    let test_cases = [
        ("simple mapping", b"name: Alice\nage: 30" as &[u8]),
        ("nested mapping", b"person:\n  name: Alice\n  age: 30"),
        ("sequence", b"- item1\n- item2\n- item3"),
        ("mixed", b"users:\n  - name: Alice\n  - name: Bob"),
    ];

    for (name, yaml) in test_cases {
        println!("\n=== {} ===", name);

        let index = YamlIndex::build(yaml).unwrap();
        let bp = index.bp();

        // Collect IB positions
        let mut ib_positions = Vec::new();
        for i in 0..yaml.len() {
            let rank = index.ib_rank1(i + 1);
            let prev_rank = index.ib_rank1(i);
            if rank > prev_rank {
                ib_positions.push(i);
            }
        }

        println!("Testing formula: open_idx = ib_idx + containers_before(bp_pos)");

        for (ib_idx, &text_pos) in ib_positions.iter().enumerate() {
            // Find BP position (deepest match)
            let mut bp_match = None;
            for bp_pos in 0..bp.len() {
                if bp.is_open(bp_pos) {
                    if let Some(bp_text) = index.bp_to_text_pos(bp_pos) {
                        if bp_text == text_pos {
                            bp_match = Some(bp_pos);
                        } else if bp_text > text_pos {
                            break;
                        }
                    }
                }
            }

            let bp_pos = bp_match.unwrap();
            let open_idx = bp.rank1(bp_pos);
            let containers_before = index.count_containers_before(bp_pos);

            // The formula
            let computed_open_idx = ib_idx + containers_before;

            let matches = computed_open_idx == open_idx;
            println!(
                "  IB[{}]: open_idx={}, ib_idx + containers_before = {} + {} = {} {}",
                ib_idx,
                open_idx,
                ib_idx,
                containers_before,
                computed_open_idx,
                if matches { "✓" } else { "✗" }
            );

            assert_eq!(
                computed_open_idx, open_idx,
                "Formula failed for IB[{}] in {}",
                ib_idx, name
            );
        }
    }

    println!("\n=== All formulas verified! ===");
}

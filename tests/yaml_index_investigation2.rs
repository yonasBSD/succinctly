//! Deeper investigation - can we compute BP pos from IB idx?

use succinctly::yaml::YamlIndex;

#[test]
fn investigate_formula() {
    // Test multiple YAML structures
    let test_cases = [
        ("simple mapping", b"name: Alice\nage: 30" as &[u8]),
        ("nested mapping", b"person:\n  name: Alice\n  age: 30"),
        ("sequence", b"- item1\n- item2\n- item3"),
        ("mixed", b"users:\n  - name: Alice\n  - name: Bob"),
    ];

    for (name, yaml) in test_cases {
        println!("\n=== {} ===", name);
        println!("YAML: {:?}", std::str::from_utf8(yaml).unwrap());

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

        // Collect BP open info
        println!("\nBP opens:");
        for bp_pos in 0..bp.len() {
            if bp.is_open(bp_pos) {
                let open_idx = bp.rank1(bp_pos);
                let text_pos = index.bp_to_text_pos(bp_pos).unwrap_or(9999);
                let is_container = index.is_container(bp_pos);
                let is_seq_item = index.is_seq_item(bp_pos);
                let c = if text_pos < yaml.len() {
                    yaml[text_pos] as char
                } else {
                    '?'
                };
                println!(
                    "  open_idx={}, bp_pos={:2}, text={:2} '{}', container={}, seq_item={}",
                    open_idx, bp_pos, text_pos, c, is_container, is_seq_item
                );
            }
        }

        // For each IB, find BP and check pattern
        println!("\nIB to BP analysis:");
        for (ib_idx, &text_pos) in ib_positions.iter().enumerate() {
            // Find BP position by scanning (current slow method)
            let mut bp_match = None;
            for bp_pos in 0..bp.len() {
                if bp.is_open(bp_pos) {
                    if let Some(bp_text) = index.bp_to_text_pos(bp_pos) {
                        if bp_text == text_pos {
                            // Take the last (deepest) match
                            bp_match = Some(bp_pos);
                        } else if bp_text > text_pos {
                            break;
                        }
                    }
                }
            }

            // What's the open_idx for this BP?
            let open_idx = bp_match.map(|bp_pos| bp.rank1(bp_pos));

            // Count containers and seq_items before this BP
            let containers_before = bp_match
                .map(|bp_pos| index.count_containers_before(bp_pos))
                .unwrap_or(0);
            let seq_items_before = bp_match
                .map(|bp_pos| index.count_seq_items_before(bp_pos))
                .unwrap_or(0);

            println!(
                "  IB[{}] text={:2} => BP={:?}, open_idx={:?}, containers={}, seq_items={}",
                ib_idx, text_pos, bp_match, open_idx, containers_before, seq_items_before
            );
        }
    }
}

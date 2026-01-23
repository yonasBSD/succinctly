//! Check if bp_to_text is suitable for binary search

use succinctly::yaml::YamlIndex;

#[test]
fn check_bp_to_text_order() {
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

        // Get bp_to_text values in order
        println!("bp_to_text values (open_idx -> text_pos):");
        let mut prev_text = 0;
        let mut is_monotonic = true;

        for bp_pos in 0..bp.len() {
            if bp.is_open(bp_pos) {
                let open_idx = bp.rank1(bp_pos);
                let text_pos = index.bp_to_text_pos(bp_pos).unwrap_or(9999);
                let c = if text_pos < yaml.len() {
                    yaml[text_pos] as char
                } else {
                    '?'
                };

                let order = if text_pos >= prev_text { "≥" } else { "<" };
                if text_pos < prev_text {
                    is_monotonic = false;
                }

                println!(
                    "  open_idx={}: text={:2} '{}' {} prev",
                    open_idx, text_pos, c, order
                );
                prev_text = text_pos;
            }
        }

        println!("Monotonically increasing: {}", is_monotonic);
    }
}

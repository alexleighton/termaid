//! stdin -> box-art filter over the vendored upstream `mermaid.rs`.
//!
//! Reads a Mermaid diagram on stdin and prints the plain-text rendered lines,
//! matching what the upstream terminal renderer produces. Used to generate
//! golden fixtures for the OCaml port.
//!
//! Width: `--width N` caps output at N columns; `--width none` disables the cap;
//! the default is 120, matching the upstream test helper.

mod mermaid;

use std::io::Read;

use mermaid::{render, MermaidStyles};
use ratatui::style::Style;

fn default_styles() -> MermaidStyles {
    let s = Style::default();
    MermaidStyles {
        border: s,
        node_text: s,
        edge: s,
        edge_label: s,
        title: s,
    }
}

fn main() {
    let mut max_width: Option<usize> = Some(120);
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        if arg == "--width" {
            match args.next().as_deref() {
                Some("none") => max_width = None,
                Some(n) => max_width = Some(n.parse().expect("invalid --width value")),
                None => panic!("--width expects an argument"),
            }
        } else {
            panic!("unknown argument: {arg}");
        }
    }

    let mut src = String::new();
    std::io::stdin()
        .read_to_string(&mut src)
        .expect("failed to read stdin");

    match render(&src, &default_styles(), max_width) {
        Some(art) => {
            for line in art.plain_lines {
                println!("{line}");
            }
        }
        None => {}
    }
}

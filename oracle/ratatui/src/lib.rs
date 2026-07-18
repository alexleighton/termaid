//! Minimal no-op shim of the ratatui surface used by the vendored `mermaid.rs`.
//! Only the plain-text render path matters to the oracle, so the styled types
//! carry no data; they exist purely so `mermaid.rs` compiles unmodified.

pub mod style {
    #[derive(Clone, Copy, Default)]
    pub struct Style;

    impl Style {
        pub fn add_modifier(self, _m: Modifier) -> Self {
            self
        }
    }

    #[derive(Clone, Copy, Default)]
    pub struct Modifier;

    impl Modifier {
        pub const ITALIC: Modifier = Modifier;
        pub const BOLD: Modifier = Modifier;
    }
}

pub mod text {
    use crate::style::Style;
    use std::marker::PhantomData;

    #[derive(Clone)]
    pub struct Span<'a>(PhantomData<&'a ()>);

    impl<'a> Span<'a> {
        pub fn styled<T: Into<String>>(_content: T, _style: Style) -> Span<'a> {
            Span(PhantomData)
        }
    }

    #[derive(Clone)]
    pub struct Line<'a>(PhantomData<&'a ()>);

    // ratatui exposes both `From<Span>` and `From<Vec<Span>>` for `Line`;
    // mermaid.rs relies on each (single-span and multi-span rows).
    impl<'a> From<Span<'a>> for Line<'a> {
        fn from(_s: Span<'a>) -> Line<'a> {
            Line(PhantomData)
        }
    }

    impl<'a> From<Vec<Span<'a>>> for Line<'a> {
        fn from(_s: Vec<Span<'a>>) -> Line<'a> {
            Line(PhantomData)
        }
    }
}

use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

fn main() {
    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    gen_phf(&out_dir);
}

fn gen_phf(out_dir: &Path) {
    let mut kwds = phf_codegen::Map::new();
    let kwds = kwds
        // Alphabetical keywords:
        .entry("...", "TokenKind::Ellipsis")
        .entry("False", "TokenKind::False")
        .entry("None", "TokenKind::None")
        .entry("True", "TokenKind::True")
        // more so "standard" keywords
        .entry("and", "TokenKind::And")
        .entry("as", "TokenKind::As")
        .entry("assert", "TokenKind::Assert")
        .entry("async", "TokenKind::Async")
        .entry("await", "TokenKind::Await")
        .entry("break", "TokenKind::Break")
        .entry("case", "TokenKind::Case")
        .entry("class", "TokenKind::Class")
        .entry("continue", "TokenKind::Continue")
        .entry("def", "TokenKind::Def")
        .entry("del", "TokenKind::Del")
        .entry("elif", "TokenKind::Elif")
        .entry("else", "TokenKind::Else")
        .entry("except", "TokenKind::Except")
        .entry("finally", "TokenKind::Finally")
        .entry("for", "TokenKind::For")
        .entry("from", "TokenKind::From")
        .entry("global", "TokenKind::Global")
        .entry("if", "TokenKind::If")
        .entry("import", "TokenKind::Import")
        .entry("in", "TokenKind::In")
        .entry("is", "TokenKind::Is")
        .entry("lambda", "TokenKind::Lambda")
        .entry("match", "TokenKind::Match")
        .entry("nonlocal", "TokenKind::Nonlocal")
        .entry("not", "TokenKind::Not")
        .entry("or", "TokenKind::Or")
        .entry("pass", "TokenKind::Pass")
        .entry("raise", "TokenKind::Raise")
        .entry("return", "TokenKind::Return")
        .entry("try", "TokenKind::Try")
        .entry("while", "TokenKind::While")
        .entry("with", "TokenKind::With")
        .entry("yield", "TokenKind::Yield")
        .build();
    writeln!(
        BufWriter::new(File::create(out_dir.join("keywords.rs")).unwrap()),
        "{kwds}",
    )
    .unwrap();
}

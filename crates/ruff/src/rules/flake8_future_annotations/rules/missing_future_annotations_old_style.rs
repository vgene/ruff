use rustpython_parser::ast::{Expr, Ranged};

use ruff_diagnostics::{Diagnostic, Violation};
use ruff_macros::{derive_message_formats, violation};
use ruff_python_ast::call_path::format_call_path;

use crate::checkers::ast::Checker;

/// ## What it does
/// Checks for missing `from __future__ import annotations` imports upon
/// detecting type annotations that can be written more succinctly under
/// PEP 563.
///
/// ## Why is this bad?
/// PEP 563 enabled the use of a number of convenient type annotations, such as
/// `list[str]` instead of `List[str]`, or `str | None` instead of
/// `Optional[str]`. However, these annotations are only available on Python
/// 3.9 and higher, _unless_ the `from __future__ import annotations` import is present.
///
/// By adding the `__future__` import, the pyupgrade rules can automatically
/// migrate existing code to use the new syntax, even for older Python versions.
/// This rule thus pairs well with pyupgrade and with Ruff's pyupgrade rules.
///
/// ## Example
/// ```python
/// from typing import List, Dict, Optional
///
///
/// def func(obj: Dict[str, Optional[int]]) -> None:
///     ...
/// ```
///
/// Use instead:
/// ```python
/// from __future__ import annotations
///
/// from typing import List, Dict, Optional
///
///
/// def func(obj: Dict[str, Optional[int]]) -> None:
///     ...
/// ```
///
/// After running the additional pyupgrade rules:
/// ```python
/// from __future__ import annotations
///
///
/// def func(obj: dict[str, int | None]) -> None:
///     ...
/// ```
#[violation]
pub struct MissingFutureAnnotationsImportOldStyle {
    name: String,
}

impl Violation for MissingFutureAnnotationsImportOldStyle {
    #[derive_message_formats]
    fn message(&self) -> String {
        let MissingFutureAnnotationsImportOldStyle { name } = self;
        format!("Missing `from __future__ import annotations`, but uses `{name}`")
    }
}

/// FA100
pub(crate) fn missing_future_annotations_old_style(checker: &mut Checker, expr: &Expr) {
    let name = checker
        .semantic_model()
        .resolve_call_path(expr)
        .map(|binding| format_call_path(&binding));

    if let Some(name) = name {
        checker.diagnostics.push(Diagnostic::new(
            MissingFutureAnnotationsImportOldStyle { name },
            expr.range(),
        ));
    }
}
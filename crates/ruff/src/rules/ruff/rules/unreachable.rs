use std::fmt;

use ruff_diagnostics::{Diagnostic, Violation};
use ruff_macros::{derive_message_formats, violation};
use ruff_python_ast::prelude::ast::text_size::TextRange;
use ruff_python_ast::prelude::ast::{
    Expr, Identifier, Ranged, Stmt, StmtAsyncFor, StmtFor, StmtWhile,
};

use crate::checkers::ast::Checker;

/// ## What it does
/// Checks for unreachable code.
///
/// ## Why is this bad?
/// Unreachable code can be a maintenance burden without ever being used.
///
/// ## Example
/// ```python
/// def function()
///     if False
///         return 0
///     return 1
/// ```
///
/// Use instead:
/// ```python
/// def function()
///     return 1
/// ```
/* TODO.
/// ## Options
/// - `pep8-naming.classmethod-decorators`
/// - `pep8-naming.staticmethod-decorators`
/// - `pep8-naming.ignore-names`
///
/// [PEP 8]: https://peps.python.org/pep-0008/#function-and-method-arguments
*/
#[violation]
pub struct UnreachableCode {
    name: String,
}

impl Violation for UnreachableCode {
    #[derive_message_formats]
    fn message(&self) -> String {
        let UnreachableCode { name } = self;
        format!("Unreachable code in {name}")
    }
}

#[allow(dead_code, unused_variables)] // TODO.
pub(crate) fn in_function(checker: &Checker, name: &Identifier, body: &[Stmt]) -> Vec<Diagnostic> {
    let basic_blocks = BasicBlocks::from(body);

    // Check:
    // Always taken (e.g. if True)
    // Never taken (e.g. if False)
    // Always return (e.g. return)

    dbg!(&basic_blocks);

    // TODO.
    return vec![Diagnostic::new(
        UnreachableCode {
            name: name.as_str().to_owned(),
        },
        body.last().unwrap().range(),
    )];
}

/// Collection of basic block.
#[derive(Debug, PartialEq)]
struct BasicBlocks<'stmt> {
    /// # Notes
    ///
    /// The order of these block is unspecified. However it's guaranteed that
    /// the last block is the first block in the function and the first block is
    /// the last statement. The block are more or less in reverse order, but it
    /// gets fussy around control flow statements (e.g. `if` statements).
    ///
    /// For loop, and similar recurring control flows, blocks the end of the
    /// body will point to the loop block again (to create the loop). However an
    /// oddity here is that this block might contain statements before which, of
    /// course, won't be executed again.
    ///
    /// For example:
    /// ```python
    /// i = 0          # block 0
    /// while True:    #
    ///     continue   # block 1
    /// ```
    /// Will create a connection between block 1 (loop body) and block 0, which
    /// includes the `i = 0` statement.
    ///
    /// To keep `NextBlock` simple(r) `NextBlock::If`'s `next` and `orelse`
    /// fields only use `BlockIndex`, which means that they can't terminate
    /// themselves. To support this we insert *empty*/fake blocks before the end
    /// of the function that we can link to.
    blocks: Vec<BasicBlock<'stmt>>,
}

impl<'stmt> From<&'stmt [Stmt]> for BasicBlocks<'stmt> {
    /// # Notes
    ///
    /// This assumes that `stmts` is a function body.
    fn from(stmts: &'stmt [Stmt]) -> BasicBlocks<'stmt> {
        let mut blocks = Vec::with_capacity(stmts.len());

        create_blocks(&mut blocks, stmts);

        if blocks.is_empty() {
            blocks.push(BasicBlock::empty());
        }

        BasicBlocks { blocks }
    }
}

/// Basic code block, sequence of statements unconditionally executed
/// "together".
#[derive(Debug, PartialEq)]
struct BasicBlock<'stmt> {
    stmts: &'stmt [Stmt],
    next: NextBlock<'stmt>,
}

/// Edge between basic blocks (in the control-flow graph).
#[derive(Debug, PartialEq)]
enum NextBlock<'stmt> {
    /// Always continue with a block.
    Always(BlockIndex),
    /// Condition jump.
    If {
        /// Condition that needs to be evaluated to jump to the `next` or
        /// `orelse` block.
        ///
        /// For conditional statements, such as `if` or `while`, this should
        /// evaluate to a boolean. However this doesn't always have to be the
        /// case, for example for `for` statements this will be iterator (e.g.
        /// for `i in range(10)` this will be `range(10)`).
        condition: &'stmt Expr,
        /// Next block if `condition` is true.
        next: BlockIndex,
        /// Next block if `condition` is false.
        orelse: BlockIndex,
    },
    /// The end.
    Terminate,
}

/// Index into [`BasicBlocks::blocks`].
type BlockIndex = usize;

impl<'stmt> BasicBlock<'stmt> {
    /// Returns an empty block that terminates.
    fn empty() -> BasicBlock<'static> {
        BasicBlock {
            stmts: &[],
            next: NextBlock::Terminate,
        }
    }
}

/// Creates basic blocks from `stmts` and appends them to `blocks`.
fn create_blocks<'stmt>(blocks: &mut Vec<BasicBlock<'stmt>>, stmts: &'stmt [Stmt]) {
    // We process the statements in reverse so that we can always point to the
    // next block (as that should always be processed).
    let mut stmts_iter = stmts.iter().enumerate().rev().peekable();
    while let Some((i, stmt)) = stmts_iter.next() {
        let next = match stmt {
            // Statements that continue to the next statement after execution.
            Stmt::FunctionDef(_)
            | Stmt::AsyncFunctionDef(_)
            | Stmt::Import(_)
            | Stmt::ImportFrom(_)
            | Stmt::ClassDef(_)
            | Stmt::Global(_)
            | Stmt::Nonlocal(_)
            | Stmt::Delete(_)
            | Stmt::Assign(_)
            | Stmt::AugAssign(_)
            | Stmt::AnnAssign(_)
            | Stmt::Expr(_)
            | Stmt::Pass(_) => unconditional_next_block(&blocks),
            // Statements that (can) divert the control flow.
            Stmt::If(stmt) => {
                let next_after_block =
                    maybe_next_block_index(blocks, || needs_next_block(&stmt.body));
                let orelse_after_block =
                    maybe_next_block_index(blocks, || needs_next_block(&stmt.orelse));
                let next = create_blocks_if_not_empty(blocks, &stmt.body, next_after_block);
                let orelse = create_blocks_if_not_empty(blocks, &stmt.orelse, orelse_after_block);
                NextBlock::If {
                    condition: &stmt.test,
                    next,
                    orelse,
                }
            }
            Stmt::While(StmtWhile {
                test: condition,
                body,
                orelse,
                ..
            })
            | Stmt::For(StmtFor {
                iter: condition,
                body,
                orelse,
                ..
            })
            | Stmt::AsyncFor(StmtAsyncFor {
                iter: condition,
                body,
                orelse,
                ..
            }) => {
                let after_block = maybe_next_block_index(blocks, || orelse.is_empty());
                // NOTE: a while loop's body must not be empty, so we can safely
                // create at least one block from it.
                debug_assert!(!body.is_empty());
                create_blocks(blocks, body);
                let next = blocks.len() - 1;
                let orelse = create_blocks_if_not_empty(blocks, orelse, after_block);
                // `create_blocks` always continues to the next block by
                // default. However in a while loop we want to continue with the
                // while block (we're about to create) to create the loop.
                // NOTE: `blocks.len()` is an invalid index at time of creation
                // as it points to the block which we're about to create.
                change_next_block(blocks, next, after_block, blocks.len());
                NextBlock::If {
                    condition,
                    next,
                    orelse,
                }
            }
            Stmt::Try(stmt) => {
                // TODO.
                todo!("Try: stmt: {stmt:#?}");
            }
            Stmt::TryStar(stmt) => {
                // TODO.
                todo!("TryStar: stmt: {stmt:#?}");
            }
            Stmt::With(stmt) => {
                // TODO.
                todo!("With: stmt: {stmt:#?}");
            }
            Stmt::AsyncWith(stmt) => {
                // TODO.
                todo!("AsyncWith: stmt: {stmt:#?}");
            }
            Stmt::Match(stmt) => {
                // TODO.
                todo!("Match: stmt: {stmt:#?}");
            }
            Stmt::Raise(stmt) => {
                // TODO.
                todo!("Raise: stmt: {stmt:#?}");
            }
            Stmt::Break(stmt) => {
                // TODO.
                todo!("Break: stmt: {stmt:#?}");
            }
            Stmt::Continue(stmt) => {
                // TODO.
                todo!("Continue: stmt: {stmt:#?}");
            }
            Stmt::Assert(stmt) => {
                // TODO.
                todo!("Assert: stmt: {stmt:#?}");
            }
            Stmt::Return(_) => NextBlock::Terminate,
        };

        // Include any statements in the block that don't divert the control flow.
        let mut start = i;
        let end = i + 1;
        while let Some(_) = stmts_iter.next_if(|(_, stmt)| !is_control_flow_stmt(stmt)) {
            start -= 1;
        }

        let block = BasicBlock {
            stmts: &stmts[start..end],
            next,
        };
        blocks.push(block);
    }
}

/// If `stmts` is not empty this calls [`create_blocks`] and returns this first
/// block reached (i.e. the last block). If `stmts` is empty this returns
/// `after` and doesn't change `blocks`.
fn create_blocks_if_not_empty<'stmt>(
    blocks: &mut Vec<BasicBlock<'stmt>>,
    stmts: &'stmt [Stmt],
    after: BlockIndex,
) -> BlockIndex {
    if stmts.is_empty() {
        after // Empty body, continue with block `after` it.
    } else {
        create_blocks(blocks, &stmts);
        blocks.len() - 1
    }
}

/// Select the next block from `blocks` unconditonally.
fn unconditional_next_block(blocks: &[BasicBlock<'_>]) -> NextBlock<'static> {
    // Either we continue with the next block (that is the last block `blocks`).
    blocks
        .len()
        .checked_sub(1)
        .map(NextBlock::Always)
        // Or it's the last statement, thus we terminate.
        .unwrap_or(NextBlock::Terminate)
}

/// Select the next block index from `blocks`. If there is not next block it
/// will add a fake/empty block if `condition` returns true. If `condition` is
/// false the returned index may not be used.
fn maybe_next_block_index(
    blocks: &mut Vec<BasicBlock<'_>>,
    condition: impl FnOnce() -> bool,
) -> BlockIndex {
    // Either we continue with the next block (that is the last block `blocks`).
    if let Some(idx) = blocks.len().checked_sub(1) {
        idx
    } else if condition() {
        // Or it's the last statement, than we force a fake end block.
        blocks.push(BasicBlock::empty());
        0
    } else {
        // NOTE: invalid, but because `condition` is false it shouldn't actually
        // be used.
        usize::MAX
    }
}

/// Change the next basic block for the block, or chain of blocks, in index
/// `fixup_index` from `expected_next` to `target`.
///
/// This doesn't change the target if it's `NextBlock::Terminate`.
fn change_next_block(
    blocks: &mut Vec<BasicBlock<'_>>,
    mut fixup_index: BlockIndex,
    expected_next: BlockIndex,
    target: BlockIndex,
) {
    loop {
        match &mut blocks[fixup_index].next {
            NextBlock::Always(next) => {
                if *next == expected_next {
                    // Found our target, change it.
                    *next = target;
                }
                return;
            }
            NextBlock::If {
                condition: _,
                next,
                orelse,
            } => {
                let next_done = if *next == expected_next {
                    // Found our target in the next branch, change it.
                    *next = target;
                    true
                } else {
                    // Follow up on the chain.
                    fixup_index = *next;
                    false
                };

                if *orelse == expected_next {
                    // Found our target in the else branch, change it.
                    *orelse = target
                } else if next_done {
                    // If we done with the next branch we only continue with the
                    // else branch.
                    fixup_index = *orelse
                } else {
                    // If we're not done with the next and else branches we need
                    // to deal with the else branch before deal with the next
                    // branch (in the next iteration).
                    let orelse = *orelse;
                    change_next_block(blocks, orelse, expected_next, target);
                }
            }
            NextBlock::Terminate => return,
        }
    }
}

/// Returns true if `stmts` need a next block, false otherwise.
fn needs_next_block(stmts: &[Stmt]) -> bool {
    // By default we assume we always need a next block.
    // Really the only cases where it's not true is if **all** statements are
    // either returns or continue, then we don't need it.
    // NOTE: that means that if we have non-return/continue statement after a
    // return/continue statement, e.g. `return 1\ni + 1`, we **do** need another
    // block as the second statements needs it.
    let mut need = true;
    for stmt in stmts {
        match stmt {
            Stmt::Return(_) | Stmt::Continue(_) => need = false,
            Stmt::FunctionDef(_)
            | Stmt::AsyncFunctionDef(_)
            | Stmt::Import(_)
            | Stmt::ImportFrom(_)
            | Stmt::ClassDef(_)
            | Stmt::Global(_)
            | Stmt::Nonlocal(_)
            | Stmt::Delete(_)
            | Stmt::Assign(_)
            | Stmt::AugAssign(_)
            | Stmt::AnnAssign(_)
            | Stmt::Expr(_)
            | Stmt::Pass(_)
            | Stmt::For(_)
            | Stmt::AsyncFor(_)
            | Stmt::While(_)
            | Stmt::If(_)
            | Stmt::With(_)
            | Stmt::AsyncWith(_)
            | Stmt::Match(_)
            | Stmt::Raise(_)
            | Stmt::Try(_)
            | Stmt::TryStar(_)
            | Stmt::Assert(_)
            | Stmt::Break(_) => return true,
        }
    }
    need
}

/// Returns true if `stmt` contains a control flow statement, e.g. an `if` or
/// `return` statement.
fn is_control_flow_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::FunctionDef(_)
        | Stmt::AsyncFunctionDef(_)
        | Stmt::Import(_)
        | Stmt::ImportFrom(_)
        | Stmt::ClassDef(_)
        | Stmt::Global(_)
        | Stmt::Nonlocal(_)
        | Stmt::Delete(_)
        | Stmt::Assign(_)
        | Stmt::AugAssign(_)
        | Stmt::AnnAssign(_)
        | Stmt::Expr(_)
        | Stmt::Pass(_) => false,
        Stmt::Return(_)
        | Stmt::For(_)
        | Stmt::AsyncFor(_)
        | Stmt::While(_)
        | Stmt::If(_)
        | Stmt::With(_)
        | Stmt::AsyncWith(_)
        | Stmt::Match(_)
        | Stmt::Raise(_)
        | Stmt::Try(_)
        | Stmt::TryStar(_)
        | Stmt::Assert(_)
        | Stmt::Break(_)
        | Stmt::Continue(_) => true,
    }
}

/// Type to create a Mermaid graph.
///
/// To learn amount Mermaid see <https://mermaid.js.org/intro>, for the syntax
/// see <https://mermaid.js.org/syntax/flowchart.html>.
struct MermaidGraph<'stmt, 'source> {
    graph: &'stmt BasicBlocks<'stmt>,
    source: &'source str,
    /// The range of the `source` code which contains the function, used to
    /// print the source code.
    range: TextRange,
}

impl<'stmt, 'source> fmt::Display for MermaidGraph<'stmt, 'source> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Include the entire source code for debugging purposes.
        write!(f, "%% Source code:\n")?;
        for line in self.source[self.range].lines() {
            write!(f, "%% {line}\n")?;
        }
        if !self.source.is_empty() {
            write!(f, "\n")?;
        }

        // Flowchart type of graph, top down.
        write!(f, "flowchart TD\n")?;

        // List all blocks.
        write!(f, "  start((\"Start\"))\n")?;
        write!(f, "  return((\"End\"))\n")?;
        for (i, block) in self.graph.blocks.iter().enumerate() {
            write!(f, "  block{i}[\"")?;
            for stmt in block.stmts {
                let code_line = &self.source[stmt.range()].trim();
                mermaid_write_qouted_str(f, code_line)?;
                write!(f, "\\n")?;
            }
            if block.stmts.is_empty() {
                write!(f, "`*(empty)*`")?;
            }
            write!(f, "\"]\n")?;
        }
        write!(f, "\n")?;

        // Then link all the blocks.
        write!(f, "  start --> block{}\n", self.graph.blocks.len() - 1)?;
        for (i, block) in self.graph.blocks.iter().enumerate().rev() {
            match block.next {
                NextBlock::Always(target) => write!(f, "  block{i} --> block{target}\n")?,
                NextBlock::If {
                    condition,
                    next,
                    orelse,
                } => {
                    let condition_code = &self.source[condition.range()].trim();
                    write!(f, "  block{i} -- \"{condition_code}\" --> block{next}\n")?;
                    write!(f, "  block{i} -- \"else\" --> block{orelse}\n")?;
                }
                NextBlock::Terminate => write!(f, "  block{i} --> return\n")?,
            }
        }

        Ok(())
    }
}

/// Escape double qoutes (`"`) in `value` using `#quot;`.
fn mermaid_write_qouted_str(f: &mut fmt::Formatter<'_>, value: &str) -> fmt::Result {
    let mut parts = value.split('"');
    if let Some(v) = parts.next() {
        write!(f, "{v}")?;
    }
    for v in parts {
        write!(f, "#quot;{v}")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use ruff_python_ast::prelude::{parse, Mode};
    use test_case::test_case;

    use crate::rules::ruff::rules::unreachable::{BasicBlocks, MermaidGraph, NextBlock};

    #[test_case("simple.py")]
    #[test_case("if.py")]
    #[test_case("while.py")]
    #[test_case("for.py")]
    #[test_case("async-for.py")]
    #[test_case("try.py")]
    fn control_flow_graph(filename: &str) {
        let path = PathBuf::from_iter(["resources/test/fixtures/control-flow-graph", filename]);
        let source = fs::read_to_string(&path).expect("failed to read file");
        let stmts = parse(&source, Mode::Module, filename)
            .unwrap_or_else(|err| panic!("failed to parse source: '{source}': {err}"))
            .expect_module()
            .body;

        for (i, stmts) in stmts.into_iter().enumerate() {
            let func = stmts.function_def_stmt().expect("statement not a function");

            let got = BasicBlocks::from(&*func.body);
            // Basic sanity checks.
            assert!(!got.blocks.is_empty(), "basic blocks should never be empty");
            assert!(
                got.blocks.first().unwrap().next == NextBlock::Terminate,
                "first block should always terminate"
            );
            // All block index should be valid.
            let valid = got.blocks.len();
            for block in &got.blocks {
                match block.next {
                    NextBlock::Always(index) => assert!(index <= valid, "invalid block index"),
                    NextBlock::If { next, orelse, .. } => {
                        assert!(next <= valid, "invalid next block index");
                        assert!(orelse <= valid, "invalid orelse block index");
                    }
                    NextBlock::Terminate => {}
                }
            }

            let got_mermaid = MermaidGraph {
                graph: &got,
                source: &source,
                range: func.range,
            }
            .to_string();
            let snapshot = format!("{filename}_{i}");
            insta::with_settings!({ omit_expression => true }, {
                insta::assert_snapshot!(snapshot, got_mermaid);
            });
        }
    }
}

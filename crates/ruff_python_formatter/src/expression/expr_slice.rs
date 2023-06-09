use crate::comments::{dangling_comments, Comments};
use crate::expression::parentheses::{
    default_expression_needs_parentheses, NeedsParentheses, Parentheses, Parenthesize,
};
use crate::trivia::Token;
use crate::trivia::{first_non_trivia_token, TokenKind};
use crate::{AsFormat, FormatNodeRule, PyFormatter};
use ruff_formatter::prelude::{hard_line_break, space, text};
use ruff_formatter::{Format, FormatError, FormatResult};
use ruff_python_ast::node::AstNode;
use ruff_python_ast::prelude::{Expr, Ranged};
use ruff_text_size::TextRange;
use rustpython_parser::ast::ExprSlice;

#[derive(Default)]
pub struct FormatExprSlice;

impl FormatNodeRule<ExprSlice> for FormatExprSlice {
    /// This implementation deviates from black in that comments are attached to the section of the
    /// slice they originate in
    fn fmt_fields(&self, item: &ExprSlice, f: &mut PyFormatter) -> FormatResult<()> {
        // `[lower:upper:step]`
        let ExprSlice {
            range,
            lower,
            upper,
            step,
        } = item;

        let (first_colon, second_colon) =
            find_colons(f.context().contents(), *range, lower, upper)?;

        // Handle comment placement
        // In placements.rs, we marked comment for None nodes a dangling and associated all others
        // as leading or dangling wrt to a node. That means we either format a node and only have
        // to handle newlines and spacing, or the node is None and we insert the corresponding
        // slice of dangling comments
        let comments = f.context().comments().clone();
        let slice_dangling_comments = comments.dangling_comments(item.as_any_node_ref());
        // Put the dangling comments (where the nodes are missing) into buckets
        let first_colon_partition_index = slice_dangling_comments
            .partition_point(|x| x.slice().start() < first_colon.range.start());
        let second_colon_partition_index =
            second_colon
                .as_ref()
                .map_or(slice_dangling_comments.len(), |second_colon| {
                    slice_dangling_comments
                        .partition_point(|x| x.slice().start() < second_colon.range.start())
                });
        // Ensure there a no dangling comments for a node if the node is present
        debug_assert!(!(lower.is_some() && first_colon_partition_index > 0));
        debug_assert!(
            !(upper.is_some() && first_colon_partition_index < second_colon_partition_index)
        );
        debug_assert!(
            !(step.is_some() && second_colon_partition_index < slice_dangling_comments.len())
        );

        // Handle spacing around the colon(s)
        // https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#slices
        let lower_simple = lower.as_ref().map_or(true, |expr| is_simple_expr(expr));
        let upper_simple = upper.as_ref().map_or(true, |expr| is_simple_expr(expr));
        let step_simple = step.as_ref().map_or(true, |expr| is_simple_expr(expr));
        let all_simple = lower_simple && upper_simple && step_simple;

        // lower
        if let Some(lower) = lower {
            lower.format().fmt(f)?;
            let lower_trailing_comments = comments.trailing_comments(lower.as_ref().into());
            if !lower_trailing_comments.is_empty() {
                // make sure the colon is after the comments
                hard_line_break().fmt(f)?;
            }
        } else {
            dangling_comments(&slice_dangling_comments[..first_colon_partition_index]).fmt(f)?;
        }

        // First colon
        // The spacing after the colon depends on both the lhs and the rhs:
        // ```
        // e00 = x[:]
        // e01 = x[:1]
        // e02 = x[: a()]
        // e10 = x[1:]
        // e11 = x[1:1]
        // e12 = x[1 : a()]
        // e20 = x[a() :]
        // e21 = x[a() : 1]
        // e22 = x[a() : a()]
        // e200 = "e"[a() : :]
        // e201 = "e"[a() :: 1]
        // e202 = "e"[a() :: a()]
        // ```
        if !all_simple {
            space().fmt(f)?;
        }
        text(":").fmt(f)?;
        // No upper node, no need for a space, e.g. `x[a() :]`
        if !all_simple && upper.is_some() {
            space().fmt(f)?;
        }

        // Upper
        if let Some(upper) = upper {
            let upper_leading_comments = comments.leading_comments(upper.as_ref().into());
            if !upper_leading_comments.is_empty() {
                // We can likely do this better by putting two spaces here if the comment was
                // on the same line as the colon in the source, but this is not a bad formatting
                // for now either
                hard_line_break().fmt(f)?;
            }
            upper.format().fmt(f)?;
            let upper_trailing_comments = comments.trailing_comments(upper.as_ref().into());
            if !upper_trailing_comments.is_empty() {
                // make sure the colon is after the comments
                hard_line_break().fmt(f)?;
            }
        } else {
            let dangling =
                &slice_dangling_comments[first_colon_partition_index..second_colon_partition_index];
            if !dangling.is_empty() {
                // put the colon and comments on their own lines
                hard_line_break().fmt(f)?;
            }
            dangling_comments(dangling).fmt(f)?;
        }

        // (optionally) step
        if second_colon.is_some() {
            // Same spacing rules as for the first colon, except for the strange case when the
            // second colon exists, but neither upper nor step
            // ```
            // e200 = "e"[a() : :]
            // e201 = "e"[a() :: 1]
            // e202 = "e"[a() :: a()]
            // ```
            if !all_simple && (upper.is_some() || step.is_none()) {
                space().fmt(f)?;
            }
            text(":").fmt(f)?;
            // No step node, no need for a space
            if !all_simple && step.is_some() {
                space().fmt(f)?;
            }
            if let Some(step) = step {
                let step_leading_comments = comments.leading_comments(step.as_ref().into());
                if !step_leading_comments.is_empty() {
                    hard_line_break().fmt(f)?;
                }
                step.format().fmt(f)?;
            } else {
                let dangling = &slice_dangling_comments[second_colon_partition_index..];
                if !dangling.is_empty() {
                    // Put the colon and comments on their own lines
                    hard_line_break().fmt(f)?;
                }
                dangling_comments(dangling).fmt(f)?;
            }
        } else {
            debug_assert!(step.is_none(), "step can't exist without a second colon");
        }
        Ok(())
    }
}

/// We're in a slice, so we know there's a first colon, but with have to look into the source
/// to find out whether there is a second one, too, e.g. `[1:2]` and `[1:10:2]`.
///
/// Returns the first and optionally the second colon.
pub(crate) fn find_colons(
    contents: &str,
    range: TextRange,
    lower: &Option<Box<Expr>>,
    upper: &Option<Box<Expr>>,
) -> FormatResult<(Token, Option<Token>)> {
    let after_lower = lower
        .as_ref()
        .map_or(range.start(), |lower| lower.range().end());
    let first_colon =
        first_non_trivia_token(after_lower, contents).ok_or(FormatError::SyntaxError)?;
    if first_colon.kind != TokenKind::Colon {
        return Err(FormatError::SyntaxError);
    }

    let after_upper = upper
        .as_ref()
        .map_or(first_colon.end(), |upper| upper.range().end());
    // At least the closing bracket must exist, so there must be a token there
    let next_token =
        first_non_trivia_token(after_upper, contents).ok_or(FormatError::SyntaxError)?;
    let second_colon = if next_token.kind == TokenKind::Colon {
        debug_assert!(
            next_token.range.start() < range.end(),
            "The next token in a slice must either be a colon or the closing bracket"
        );
        Some(next_token)
    } else {
        None
    };
    Ok((first_colon, second_colon))
}

/// Determines whether this expression needs a space around the colon
/// <https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#slices>
fn is_simple_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::Constant(_) | Expr::Name(_))
}

pub(crate) enum ExprSliceCommentSection {
    Lower,
    Upper,
    Step,
}

/// Assigns a comment to lower/upper/step in `[lower:upper:step]`.
///
/// ```python
/// "sliceable"[
///     # lower comment
///     :
///     # upper comment
///     :
///     # step comment
/// ]
/// ```
pub(crate) fn assign_comment_in_slice(
    comment: TextRange,
    contents: &str,
    expr_slice: &ExprSlice,
) -> ExprSliceCommentSection {
    let ExprSlice {
        range,
        lower,
        upper,
        step: _,
    } = expr_slice;

    let (first_colon, second_colon) = find_colons(contents, *range, lower, upper)
        .expect("SyntaxError when trying to parse slice");

    if comment.start() < first_colon.range.start() {
        ExprSliceCommentSection::Lower
    } else {
        // We are to the right of the first colon
        if let Some(second_colon) = second_colon {
            if comment.start() < second_colon.range.start() {
                ExprSliceCommentSection::Upper
            } else {
                ExprSliceCommentSection::Step
            }
        } else {
            // No second colon means there is no step
            ExprSliceCommentSection::Upper
        }
    }
}

impl NeedsParentheses for ExprSlice {
    fn needs_parentheses(
        &self,
        parenthesize: Parenthesize,
        source: &str,
        comments: &Comments,
    ) -> Parentheses {
        default_expression_needs_parentheses(self.into(), parenthesize, source, comments)
    }
}

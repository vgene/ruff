use crate::comments::Comments;
use crate::expression::parentheses::{
    default_expression_needs_parentheses, NeedsParentheses, Parentheses, Parenthesize,
};
use crate::{AsFormat, FormatNodeRule, PyFormatter};
use ruff_formatter::prelude::{group, soft_line_break_or_space, space, text};
use ruff_formatter::{format_args, write, Buffer, FormatResult};
use rustpython_parser::ast::ExprIfExp;

#[derive(Default)]
pub struct FormatExprIfExp;

impl FormatNodeRule<ExprIfExp> for FormatExprIfExp {
    fn fmt_fields(&self, item: &ExprIfExp, f: &mut PyFormatter) -> FormatResult<()> {
        let ExprIfExp {
            range: _,
            test,
            body,
            orelse,
        } = item;

        write!(
            f,
            [group(&format_args![
                body.format(),
                soft_line_break_or_space(),
                text("if"),
                space(),
                test.format(),
                soft_line_break_or_space(),
                text("else"),
                space(),
                orelse.format(),
            ])]
        )
    }
}

impl NeedsParentheses for ExprIfExp {
    fn needs_parentheses(
        &self,
        parenthesize: Parenthesize,
        source: &str,
        comments: &Comments,
    ) -> Parentheses {
        default_expression_needs_parentheses(self.into(), parenthesize, source, comments)
    }
}

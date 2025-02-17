use crate::comments::Comments;
use crate::expression::parentheses::{
    default_expression_needs_parentheses, NeedsParentheses, Parentheses, Parenthesize,
};
use crate::{not_yet_implemented_custom_text, FormatNodeRule, PyFormatter};
use ruff_formatter::{write, Buffer, FormatResult};
use rustpython_parser::ast::ExprCall;

#[derive(Default)]
pub struct FormatExprCall;

impl FormatNodeRule<ExprCall> for FormatExprCall {
    fn fmt_fields(&self, item: &ExprCall, f: &mut PyFormatter) -> FormatResult<()> {
        if item.args.is_empty() && item.keywords.is_empty() {
            write!(
                f,
                [not_yet_implemented_custom_text("NOT_IMPLEMENTED_call()")]
            )
        } else {
            write!(
                f,
                [not_yet_implemented_custom_text(
                    "NOT_IMPLEMENTED_call(NOT_IMPLEMENTED_arg)"
                )]
            )
        }
    }
}

impl NeedsParentheses for ExprCall {
    fn needs_parentheses(
        &self,
        parenthesize: Parenthesize,
        source: &str,
        comments: &Comments,
    ) -> Parentheses {
        match default_expression_needs_parentheses(self.into(), parenthesize, source, comments) {
            Parentheses::Optional => Parentheses::Never,
            parentheses => parentheses,
        }
    }
}

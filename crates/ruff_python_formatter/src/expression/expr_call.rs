use crate::comments::{dangling_comments, Comments};
use crate::context::PyFormatContext;
use crate::expression::parentheses::{
    default_expression_needs_parentheses, NeedsParentheses, Parentheses, Parenthesize,
};
use crate::{not_yet_implemented_custom_text, AsFormat, FormatNodeRule, PyFormatter};
use ruff_formatter::formatter::Formatter;
use ruff_formatter::prelude::{
    format_with, group, soft_block_indent, soft_line_break_or_space, text,
};
use ruff_formatter::{format_args, write, Buffer, Format, FormatResult};
use rustpython_parser::ast::{ExprCall, Keyword};

#[derive(Default)]
pub struct FormatExprCall;

impl FormatNodeRule<ExprCall> for FormatExprCall {
    fn fmt_fields(&self, item: &ExprCall, f: &mut PyFormatter) -> FormatResult<()> {
        not_yet_implemented_custom_text("NOT_IMPLEMENTED_call()").fmt(f)
        /*
        let ExprCall {
            range: _,
            func,
            args,
            keywords,
        } = item;

        if args.is_empty() && keywords.is_empty() {
            let comments = f.context().comments().clone();
            let comments = comments.dangling_comments(item);
            return write!(
                f,
                [
                    func.format(),
                    text("("),
                    dangling_comments(&comments),
                    text(")")
                ]
            );
        }

        let all_args = format_with(|f| {
            f.join_with(format_args![text(","), soft_line_break_or_space()])
                .entries(
                    args.iter()
                        .map(|arg| arg.format().with_options(Parenthesize::IfBreaks)),
                )
                //.entries(keywords.iter().map(|keyword| FormatKeyword(keyword)))
                .finish()
        });

        write!(
            f,
            [
                func.format(),
                text("("),
                soft_block_indent(&group(&all_args)),
                text(")")
            ]
        )
         */
    }

    fn fmt_dangling_comments(&self, _node: &ExprCall, _f: &mut PyFormatter) -> FormatResult<()> {
        // Handled in `fmt_fields`
        Ok(())
    }
}

impl NeedsParentheses for ExprCall {
    fn needs_parentheses(
        &self,
        parenthesize: Parenthesize,
        source: &str,
        comments: &Comments,
    ) -> Parentheses {
        /*let trailing_comments = comments.trailing_comments(self.func.as_ref());
        if !trailing_comments.is_empty() {
            return Parentheses::Always;
        }*/
        match default_expression_needs_parentheses(self.into(), parenthesize, source, comments) {
            Parentheses::Optional => Parentheses::Never,
            parentheses => parentheses,
        }
    }
}

struct FormatKeyword<'a>(&'a Keyword);

impl Format<PyFormatContext<'_>> for FormatKeyword<'_> {
    fn fmt(&self, f: &mut Formatter<PyFormatContext<'_>>) -> FormatResult<()> {
        let Keyword {
            range: _,
            arg,
            value,
        } = self.0;
        let arg = arg.as_ref().expect("why is arg None plz tell me");
        write!(f, [arg.format(), text("="), value.format()])
    }
}

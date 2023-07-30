use crate::{parser::Rule, types::Span};

pub type Error = Span<String>;

impl From<pest::error::Error<Rule>> for Error {
    fn from(value: pest::error::Error<Rule>) -> Self {
        let span = match value.location {
            pest::error::InputLocation::Pos(a) => (a, a),
            pest::error::InputLocation::Span(span) => span,
        };
        let message = match value.variant {
            pest::error::ErrorVariant::ParsingError { positives, .. } => {
                let positives = positives
                    .into_iter()
                    .filter(|rule| !matches!(rule, Rule::EOI))
                    .collect::<Vec<_>>();
                format!("expected {positives:?}")
            }
            pest::error::ErrorVariant::CustomError { message } => message,
        };

        Span {
            span,
            inner: message,
        }
    }
}

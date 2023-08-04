use crate::{preprocessor, span::Spanned};

pub type Error = Spanned<String>;

impl From<pest::error::Error<preprocessor::Rule>> for Error {
    fn from(value: pest::error::Error<preprocessor::Rule>) -> Self {
        let span = match value.location {
            pest::error::InputLocation::Pos(a) => (a, a),
            pest::error::InputLocation::Span(span) => span,
        };
        let message = match value.variant {
            pest::error::ErrorVariant::ParsingError { positives, .. } => {
                let positives = positives
                    .into_iter()
                    .filter(|rule| !matches!(rule, preprocessor::Rule::EOI))
                    .collect::<Vec<_>>();
                format!("expected {positives:?}")
            }
            pest::error::ErrorVariant::CustomError { message } => message,
        };

        Spanned {
            span,
            inner: message,
        }
    }
}

pub type Span = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Spanned<R> {
        Spanned::<R> {
            inner: f(self.inner),
            span: self.span,
        }
    }
}

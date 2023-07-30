use core::iter::Peekable;
use core::marker::PhantomData;
use std::fmt::Debug;

/// Associativity of an infix binary operator, used by [`Op::infix(Assoc)`].
///
/// [`Op::infix(Assoc)`]: struct.Op.html
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc {
    /// Left operator associativity. Evaluate expressions from left-to-right.
    Left,
    /// Right operator associativity. Evaluate expressions from right-to-left.
    Right,
}

type Prec = u32;

pub enum Affix {
    Prefix,
    Postfix,
    Infix(Assoc),
}

pub trait Precedence {
    type Item: Debug;
    /// weight when symbol is evaluated as an inflix op
    fn inflix_weight(item: &Self::Item) -> Option<(Affix, Prec)>;
    /// weight when symbol is evaluated as an prefix op
    fn prefix_weight(item: &Self::Item) -> Option<(Affix, Prec)>;
}

#[derive(Default)]
pub struct PrattParser {}

impl PrattParser {
    /// Instantiate a new `PrattParser`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Maps primary expressions with a closure `primary`.
    pub fn map_primary<'a, R, X, T>(&self, primary: X) -> PrattParserMap<'a, R, X, T>
    where
        R: Precedence,
        X: FnMut(R::Item) -> T,
    {
        PrattParserMap {
            primary,
            prefix: None,
            postfix: None,
            infix: None,
            phantom: PhantomData,
        }
    }
}

type PrefixFn<'a, R, T> = Box<dyn FnMut(R, T) -> T + 'a>;
type PostfixFn<'a, R, T> = Box<dyn FnMut(T, R) -> T + 'a>;
type InfixFn<'a, R, T> = Box<dyn FnMut(T, R, T) -> T + 'a>;

/// Product of calling [`map_primary`] on [`PrattParser`], defines how expressions should
/// be mapped.
///
/// [`map_primary`]: struct.PrattParser.html#method.map_primary
/// [`PrattParser`]: struct.PrattParser.html
pub struct PrattParserMap<'a, R, F, T>
where
    R: Precedence,
    F: FnMut(R::Item) -> T,
{
    primary: F,
    prefix: Option<PrefixFn<'a, R::Item, T>>,
    postfix: Option<PostfixFn<'a, R::Item, T>>,
    infix: Option<InfixFn<'a, R::Item, T>>,
    phantom: PhantomData<T>,
}

impl<'a, R, F, T> PrattParserMap<'a, R, F, T>
where
    R: Precedence,
    F: FnMut(R::Item) -> T,
{
    /// Maps prefix operators with closure `prefix`.
    pub fn map_prefix<X>(mut self, prefix: X) -> Self
    where
        X: FnMut(R::Item, T) -> T + 'a,
    {
        self.prefix = Some(Box::new(prefix));
        self
    }

    /// Maps postfix operators with closure `postfix`.
    pub fn map_postfix<X>(mut self, postfix: X) -> Self
    where
        X: FnMut(T, R::Item) -> T + 'a,
    {
        self.postfix = Some(Box::new(postfix));
        self
    }

    /// Maps infix operators with a closure `infix`.
    pub fn map_infix<X>(mut self, infix: X) -> Self
    where
        X: FnMut(T, R::Item, T) -> T + 'a,
    {
        self.infix = Some(Box::new(infix));
        self
    }

    /// The last method to call on the provided pairs to execute the Pratt
    /// parser (previously defined using [`map_primary`], [`map_prefix`], [`map_postfix`],
    /// and [`map_infix`] methods).
    ///
    /// [`map_primary`]: struct.PrattParser.html#method.map_primary
    /// [`map_prefix`]: struct.PrattParserMap.html#method.map_prefix
    /// [`map_postfix`]: struct.PrattParserMap.html#method.map_postfix
    /// [`map_infix`]: struct.PrattParserMap.html#method.map_infix
    pub fn parse<P: Iterator<Item = R::Item>>(&mut self, pairs: P) -> T {
        self.expr(&mut pairs.peekable(), 0)
    }

    fn expr<P: Iterator<Item = R::Item>>(&mut self, pairs: &mut Peekable<P>, rbp: Prec) -> T {
        let mut lhs = self.nud(pairs);
        while rbp < self.lbp(pairs) {
            lhs = self.led(pairs, lhs);
        }
        lhs
    }

    /// Null-Denotation
    ///
    /// "the action that should happen when the symbol is encountered
    ///  as start of an expression (most notably, prefix operators)
    fn nud<P: Iterator<Item = R::Item>>(&mut self, pairs: &mut Peekable<P>) -> T {
        let pair = pairs.next().expect("Pratt parsing expects non-empty Pairs");
        match self.prefix_weight(&pair) {
            Some((Affix::Prefix, prec)) => {
                let rhs = self.expr(pairs, prec - 1);
                match self.prefix.as_mut() {
                    Some(prefix) => prefix(pair, rhs),
                    None => panic!("Could not map {:?}, no `.map_prefix(...)` specified", pair),
                }
            }
            None => (self.primary)(pair),
            _ => panic!("Expected prefix or primary expression, found {:?}", pair),
        }
    }

    /// Left-Denotation
    ///
    /// "the action that should happen when the symbol is encountered
    /// after the start of an expression (most notably, infix and postfix operators)"
    fn led<P: Iterator<Item = R::Item>>(&mut self, pairs: &mut Peekable<P>, lhs: T) -> T {
        let pair = pairs.next().unwrap();
        match self.inflix_weight(&pair) {
            Some((Affix::Infix(assoc), prec)) => {
                let rhs = match assoc {
                    Assoc::Left => self.expr(pairs, prec),
                    Assoc::Right => self.expr(pairs, prec - 1),
                };
                match self.infix.as_mut() {
                    Some(infix) => infix(lhs, pair, rhs),
                    None => panic!("Could not map {:?}, no `.map_infix(...)` specified", pair),
                }
            }
            Some((Affix::Postfix, _)) => match self.postfix.as_mut() {
                Some(postfix) => postfix(lhs, pair),
                None => panic!("Could not map {:?}, no `.map_postfix(...)` specified", pair),
            },
            _ => panic!("Expected postfix or infix expression, found {:?}", pair),
        }
    }

    /// Left-Binding-Power
    ///
    /// "describes the symbol's precedence in infix form (most notably, operator precedence)"
    fn lbp<P: Iterator<Item = R::Item>>(&self, pairs: &mut Peekable<P>) -> Prec {
        match pairs.peek() {
            Some(pair) => match self.inflix_weight(pair) {
                Some((_, prec)) => prec,
                None => panic!("Expected operator, found {:?}", pair),
            },
            None => 0,
        }
    }

    #[inline]
    fn inflix_weight(&self, pair: &R::Item) -> Option<(Affix, Prec)> {
        R::inflix_weight(pair).map(|(x, y)| (x, y + 10))
    }

    #[inline]
    fn prefix_weight(&self, pair: &R::Item) -> Option<(Affix, Prec)> {
        R::prefix_weight(pair).map(|(x, y)| (x, y + 10))
    }
}

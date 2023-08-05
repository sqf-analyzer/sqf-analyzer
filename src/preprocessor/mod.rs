use std::borrow::Cow;
use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use crate::error::Error;
use crate::span::{Span, Spanned};

pub trait ToSpan {
    fn to_span(&self) -> Span;
}

pub type SpannedRef<'a> = Spanned<Cow<'a, str>>;

impl<'a> From<Pair<'a, Rule>> for SpannedRef<'a> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Self {
            span: value.to_span(),
            inner: value.as_str().into(),
        }
    }
}

impl<'a> From<Pair<'a, Rule>> for Spanned<String> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Self {
            span: value.to_span(),
            inner: value.as_str().to_string(),
        }
    }
}

impl ToSpan for Pair<'_, Rule> {
    fn to_span(&self) -> Span {
        (self.as_span().start(), self.as_span().end())
    }
}

#[derive(Parser)]
#[grammar = "preprocessor/preprocessor.pest"]
struct PreprocessorParser;

pub fn pairs(data: &str) -> Result<Pairs<'_, Rule>, Error> {
    Ok(PreprocessorParser::parse(Rule::program, data)?)
}

/// Definition of an expression inside a #define.
#[derive(Debug, Clone)]
pub enum DefineExpr {
    Quote(Spanned<String>), // unary operator "#" used to quote terms
    Concat(Spanned<String>, Spanned<String>), // binary operator "##" used to concatenate terms
    Term(Spanned<String>),  // an arbitrary term
}

/// # Note
/// This struct cannot have an associated lifetime because it can be shared between files
/// with a different lifetime
#[derive(Debug, Clone)]
pub struct Define {
    pub name: Spanned<String>,
    pub arguments: VecDeque<Spanned<String>>,
    pub body: VecDeque<DefineExpr>,
}

pub type Defines = HashMap<String, Define>;

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Ifdef(SpannedRef<'a>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    Ifndef(SpannedRef<'a>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    If(
        VecDeque<SpannedRef<'a>>,
        VecDeque<Ast<'a>>,
        VecDeque<Ast<'a>>,
    ),
    Define(Define),
    Undefine(SpannedRef<'a>),
    Include(SpannedRef<'a>),
    Body(VecDeque<Ast<'a>>),
    Comment(SpannedRef<'a>),
    Term(SpannedRef<'a>),
}

fn parse_if(pair: Pair<'_, Rule>) -> Ast<'_> {
    let mut if_ = pair.into_inner();
    let mut if_start = if_.next().unwrap().into_inner();
    let if_body = if_.next().unwrap().into_inner();
    let if_body = (if_body.len() != 0).then_some(if_body);
    let else_ = if if_.len() > 0 {
        let _ = if_.next(); // #else
        if_.next()
            .map(|else_| else_.into_inner())
            .and_then(|else_| (else_.len() != 0).then_some(else_))
    } else {
        None
    };

    let body = if_body.map(_parse).unwrap_or_default();
    let else_ = else_.map(_parse).unwrap_or_default();

    let if_type = if_start.next().unwrap();
    match if_type.as_str() {
        "#ifndef" => {
            let name = if_start.next().unwrap();
            Ast::Ifndef(name.into(), body, else_)
        }
        "#ifdef" => {
            let name = if_start.next().unwrap();
            Ast::Ifdef(name.into(), body, else_)
        }
        _ => {
            assert!(matches!(if_type.as_rule(), Rule::if_expr));
            let expr = if_type.into_inner().map(|x| x.into()).collect();
            Ast::If(expr, body, else_)
        }
    }
}

fn define_expr(x: Pair<'_, Rule>) -> DefineExpr {
    match x.as_rule() {
        Rule::base_term => DefineExpr::Term(x.into()),
        Rule::define_concat => {
            let mut pairs = x.into_inner();
            DefineExpr::Concat(pairs.next().unwrap().into(), pairs.next().unwrap().into())
        }
        Rule::define_quote => DefineExpr::Quote(x.into()),
        other => unreachable!("{other:?}"),
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Ast<'_> {
    match pair.as_rule() {
        Rule::if_ => parse_if(pair),
        Rule::define => {
            let mut define = pair.into_inner();
            let name = define.next().unwrap().into();

            let Some(args_or_body) = define.next() else {
                return Ast::Define(Define {
                    name, arguments: Default::default(), body: Default::default()});
            };
            let is_arguments = matches!(args_or_body.as_rule(), Rule::define_arguments);

            let (arguments, body) = if is_arguments {
                let body = define
                    .filter(|x| x.as_rule() != Rule::EOI)
                    .map(define_expr)
                    .collect::<VecDeque<_>>();

                let args = args_or_body.into_inner().map(|x| x.into()).collect();

                (args, body)
            } else {
                (
                    VecDeque::new(),
                    std::iter::once(define_expr(args_or_body))
                        .chain(define.filter(|x| x.as_rule() != Rule::EOI).map(define_expr))
                        .collect(),
                )
            };

            Ast::Define(Define {
                name,
                arguments,
                body,
            })
        }
        Rule::undef => {
            let word = pair.into_inner().next().unwrap();
            Ast::Undefine(word.into())
        }
        Rule::include => {
            let word = pair.into_inner().next().unwrap();
            Ast::Include(word.into())
        }
        Rule::COMMENT => Ast::Comment(pair.into()),
        _ => Ast::Term(pair.into()),
    }
}

fn _parse(pairs: Pairs<'_, Rule>) -> VecDeque<Ast<'_>> {
    pairs
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(parse_pair)
        .collect()
}

pub fn parse(pairs: Pairs<'_, Rule>) -> Ast<'_> {
    Ast::Body(_parse(pairs))
}

#[derive(Clone)]
pub struct AstIterator<'a> {
    base: VecDeque<Ast<'a>>,
    pub state: State<'a>,
}

#[derive(Debug, Clone)]
pub struct State<'a> {
    pub defines: Defines,
    // stack of define that need to be evaluated. E.g.
    /*
    ```
    #define C(B) C(B)##1
    #define A(B) B##C(B)##1
    ```
    `A(a)` results in `aa11`

    define is the stack of each of the defines that are being evaluated
    `[], [A], [A, C], [A], []`
    */
    pub stack: VecDeque<(Define, Vec<SpannedRef<'a>>)>,
    pub include: VecDeque<Spanned<String>>,
    pub path: PathBuf,
    pub state: MacroState,
    pub errors: Vec<Error>,
}

fn evaluate_terms<'a>(
    terms: &mut VecDeque<Ast<'a>>,
    state: &mut State<'a>,
) -> (bool, Option<SpannedRef<'a>>) {
    // go to front, take (has_more, item); if not has_more; remove it
    match terms.front_mut() {
        Some(item) => {
            let (has_more, item) = take_last(item, state);
            if has_more {
                (true, item)
            } else {
                terms.pop_front();
                (true, item)
            }
        }
        None => (false, None),
    }
}

fn evaluate_if(expr: &VecDeque<SpannedRef>, defines: &Defines) -> bool {
    match expr.len() {
        1 => {
            let def_name = expr.front().unwrap().inner.clone();
            defines
                .get(def_name.as_ref())
                .and_then(|_| todo!("compute bool from variable"))
                .unwrap_or_default()
        }
        3 => {
            todo!("compute op from variable")
        }
        _ => unreachable!(),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MacroState {
    /// not in a macro
    None,
    /// found a macro and it is waiting for "("
    ParenthesisStart,
    /// consumed argument and is waiting for a coma or ")"
    Coma,
    /// consumed "(" or "," and it expects an argument
    Argument,
}

fn parse_file(
    span: Span,
    content: String,
    state: &mut State,
    path: PathBuf,
) -> Option<VecDeque<Spanned<String>>> {
    // parse into VecDeque<Spanned<String>> due to no lifetime management
    let defines = std::mem::take(&mut state.defines);

    let mut ast = match tokens(&content, defines, path.clone()) {
        Ok(ast) => ast,
        Err(e) => {
            // todo: how do we handle errors in other files? Error may need a lift to track files
            let error = Error {
                inner: format!(
                    "Error while parsing file \"{}\" at position {:?}: {}",
                    path.display(),
                    e.span,
                    e.inner
                ),
                span,
            };
            state.errors.push(error);
            return None;
        }
    };

    let tokens = ast.by_ref().map(|x| x.map(|x| x.into_owned())).collect();
    // collect errors and defines
    state.errors.extend(ast.state.errors);
    state.defines = ast.state.defines;

    Some(tokens)
}

fn process_include(name: Spanned<&str>, state: &mut State) -> Option<VecDeque<Spanned<String>>> {
    let mut path = state.path.clone();
    path.pop();
    path.push(name.inner);
    match std::fs::read_to_string(path.clone()) {
        Err(_) => {
            state.errors.push(Error {
                inner: format!("Cannot read file in path \"{}\"", path.display()),
                span: name.span,
            });
            None
        }
        Ok(content) => parse_file(name.span, content, state, path),
    }
}

fn take_last<'a>(ast: &mut Ast<'a>, state: &mut State<'a>) -> (bool, Option<SpannedRef<'a>>) {
    match ast {
        Ast::Ifndef(term, else_, then) | Ast::Ifdef(term, then, else_) => {
            if state.defines.contains_key(term.inner.as_ref()) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::If(expr, then, else_) => {
            if evaluate_if(expr, &state.defines) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::Define(define) => {
            state
                .defines
                .insert(define.name.inner.clone(), define.clone());
            (false, None)
        }
        Ast::Undefine(name) => {
            state.defines.remove(name.inner.as_ref());
            (false, None)
        }
        Ast::Include(name) => match process_include(name.as_deref().map(|x| x.as_ref()), state) {
            Some(include) => {
                state.include = include;
                pop_include(&mut state.include)
            }
            None => (false, None),
        },
        Ast::Body(terms) => evaluate_terms(terms, state),
        Ast::Term(term) => (false, Some(term.clone())),
        Ast::Comment(_) => (false, None),
    }
}

fn pop_include<'a>(stack: &mut VecDeque<Spanned<String>>) -> (bool, Option<SpannedRef<'a>>) {
    stack
        .pop_front()
        .map(|x| (stack.is_empty(), Some(x.map(|x| x.into()))))
        .unwrap_or((false, None))
}

impl<'a> AstIterator<'a> {
    pub fn new(base: Ast<'a>, defines: Defines, path: PathBuf) -> Self {
        let Ast::Body(base) = base else {
            panic!()
        };
        Self {
            base,
            state: State {
                defines,
                path,
                state: MacroState::None,
                stack: Default::default(),
                include: Default::default(),
                errors: Default::default(),
            },
        }
    }

    fn pop_define(&mut self) -> Option<SpannedRef<'a>> {
        if self.state.stack.is_empty() {
            return None;
        }
        if self.state.state != MacroState::None {
            return None;
        }

        // clear any defines that have an empty body
        self.state.stack.retain(|x| !x.0.body.is_empty());

        // get the first front item of the stack that has all arguments available
        let (define, arguments) = self
            .state
            .stack
            .iter_mut()
            .rev()
            .find(|(define, args)| define.arguments.len() == args.len())?;

        // argument replacement
        let replace = |arg: String| {
            define
                .arguments
                .iter()
                .zip(arguments.iter())
                .find(|x| x.0.inner == arg)
                .map(|x| x.1.inner.clone())
                .unwrap_or_else(|| arg.into())
        };

        // operator expansion
        define.body.pop_front().map(|x| match x {
            DefineExpr::Quote(lhs) => Spanned {
                inner: format!("\"{}\"", replace(lhs.inner)).into(),
                span: lhs.span,
            },
            DefineExpr::Concat(lhs, rhs) => Spanned {
                inner: format!(
                    "{}{}",
                    replace(lhs.inner).as_ref(),
                    replace(rhs.inner).as_ref()
                )
                .into(),
                span: (lhs.span.0, rhs.span.1),
            },
            DefineExpr::Term(term) => term.map(|x| x.into()),
        })
    }

    fn define_update(&mut self, item: &SpannedRef<'a>) -> Option<()> {
        let define = self.state.defines.get(item.inner.as_ref())?;

        // there is a define with this token => push it to the stack
        self.state.stack.push_back((define.clone(), vec![]));
        assert_eq!(self.state.state, MacroState::None);
        if !define.arguments.is_empty() {
            self.state.state = MacroState::ParenthesisStart;
        }
        // ignore the token and request another one (possibly a paranthesis for arguments)
        Some(())
    }

    fn define_arg_update(&mut self, item: &SpannedRef<'a>) -> Option<()> {
        // get the top of the stack missing arguments
        let (_, arguments) = self
            .state
            .stack
            .iter_mut()
            .rev()
            .find(|(define, arguments)| define.arguments.len() != arguments.len())?;

        arguments.push(item.clone());
        self.state.state = MacroState::Coma;
        Some(())
    }
}

impl<'a> Iterator for AstIterator<'a> {
    type Item = SpannedRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // if we are in the define state, advance it
        let (has_more, item) = if let Some(item) = self.pop_define() {
            (true, Some(item))
        } else {
            evaluate_terms(&mut self.base, &mut self.state)
        };

        if self.state.state == MacroState::Coma
            && matches!(
                item.as_ref().map(|x| x.as_deref().map(|x| x.as_ref())),
                Some(Spanned { inner: ")", .. })
            )
        {
            self.state.state = MacroState::None;
            return self.next();
        }

        if matches!(
            self.state.state,
            MacroState::ParenthesisStart | MacroState::Coma
        ) {
            self.state.state = MacroState::Argument;
            return self.next();
        }

        if let Some(item) = &item {
            if let Some(()) = self.define_update(item) {
                return self.next();
            };

            if let Some(()) = self.define_arg_update(item) {
                return self.next();
            };
        }

        match (has_more, item) {
            (true, None) => self.next(),
            (true, item) => item,
            (false, None) => None,
            (false, item) => item,
        }
    }
}

pub fn tokens(data: &str, defines: Defines, path: PathBuf) -> Result<AstIterator, Error> {
    let pairs = pairs(data)?;

    let ast = parse(pairs);
    Ok(AstIterator::new(ast, defines, path))
}

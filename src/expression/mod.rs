pub use self::{
    as_expr::AsExpr, block::BlockExpr, if_expr::IfExpr, literal::LiteralExpr,
    ty_instance::TyInstanceExpr, var::VarExpr,
};
use crate::{
    parser::token::{Literal, Token},
    types::{
        Context, EnumType, FnTy, Scheme, Stack, StackSplitError, Substitution, Ty, TyGen, Types,
        UnificationError, Var, Variance,
    },
};
use std::{convert::From, fmt::Debug};

mod as_expr;
mod block;
mod if_expr;
mod literal;
mod ty_instance;
mod var;

#[derive(Debug, Clone)]
pub enum ExprKind<'src> {
    Literal(LiteralExpr<'src>),
    Block(BlockExpr<'src>),
    Var(VarExpr<'src>),
    As(AsExpr<'src>),
    Add,
    Sub,
    LessThan,
    Equals,
    If(IfExpr<'src>),
    DotSequence(Vec<Expr<'src>>),
    TyInstance(TyInstanceExpr<'src>),
}

#[derive(Debug, Clone)]
pub struct Expr<'src> {
    pub token: Token<'src>,
    pub kind: ExprKind<'src>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ApplicationError<'src> {
    TooFewElements(Token<'src>, StackSplitError<'src>),
    UnificationError(Token<'src>, UnificationError<'src>),
    UnknownVar(Token<'src>, &'src str),
    Message(Token<'src>, String),
    Other,
}

impl<'src> Debug for ApplicationError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooFewElements(tok, StackSplitError(stack, n)) => {
                writeln!(f, "{}: Too few elements on the stack", tok.quote().loc())?;
                writeln!(
                    f,
                    "  ┏━ Expected {n} elements, but found {} instead.",
                    stack.len()
                )?;
                write!(f, "  ┖─── Stack: {stack:?}")?;
                Ok(())
            }
            Self::UnificationError(token, UnificationError::TypesNotEqual(left, right)) => {
                writeln!(f, "{}: Unification Error", token.quote().loc())?;
                write!(f, "  ━━━ Failed to unify types: `{left:?}` and `{right:?}`")
            }
            Self::UnificationError(token, UnificationError::StackLensDiffer(left, right)) => {
                writeln!(f, "{}: Unification Error", token.quote().loc())?;
                writeln!(f, "  ┏━ Branches produced stacks of different lengths")?;
                writeln!(f, "  ┠───  true: {left:?}")?;
                write!(f, "  ┖─── false: {right:?}")?;
                Ok(())
            }
            Self::UnknownVar(token, var) => {
                write!(f, "{}: Unknown Variable `{var}`", token.quote().loc())
            }
            Self::Message(token, message) => {
                write!(f, "{}: {message}", token.quote().loc())
            }
            Self::Other => write!(f, "Other"),
        }
    }
}

impl<'src> From<()> for ApplicationError<'src> {
    fn from(_: ()) -> Self {
        Self::Other
    }
}

impl<'src> Expr<'src> {
    pub fn new<K>(kind: K, token: Token<'src>) -> Self
    where
        K: Into<ExprKind<'src>>,
    {
        Self {
            token,
            kind: kind.into(),
        }
    }

    pub fn apply<'ctx>(
        self,
        stack: Stack<'src>,
        types: &Types<'src>,
        context: &mut Context<'src>,
        gen: &mut TyGen,
    ) -> Result<(Stack<'src>, Substitution<'src>), ApplicationError<'src>> {
        let (stack, subs) = match self.kind {
            ExprKind::Literal(lit) => {
                let func: FnTy<'_> = lit.into();
                func.apply(&self.token, stack)?
            }
            ExprKind::Block(block) => block.apply(stack, types, context, gen)?,
            ExprKind::Var(VarExpr(var)) => {
                let func = match context.iter().rev().find(|Var { ident, .. }| *ident == var) {
                    Some(Var { scheme, .. }) => scheme.instantiate(gen).0,
                    None => return Err(ApplicationError::UnknownVar(self.token, var)),
                };
                func.apply(&self.token, stack)?
            }
            ExprKind::As(AsExpr(bindings)) => {
                let (head, tail) = stack
                    .split(bindings.len())
                    .map_err(|e| ApplicationError::TooFewElements(self.token, e))?;
                for (ty, ident) in tail.into_iter().zip(bindings.into_iter()) {
                    context.push(ident, Scheme::new([], FnTy::new([], [ty])));
                }

                (head, Substitution::new())
            }
            ExprKind::Add => FnTy::new([Ty::U32, Ty::U32], [Ty::U32]).apply(&self.token, stack)?,
            ExprKind::Sub => FnTy::new([Ty::U32, Ty::U32], [Ty::U32]).apply(&self.token, stack)?,
            ExprKind::LessThan => {
                FnTy::new([Ty::U32, Ty::U32], [Ty::Bool]).apply(&self.token, stack)?
            }
            ExprKind::Equals => {
                let (t, var) = gen.fresh_with_var();
                FnTy::new([t.clone(), t.clone()], [Ty::Bool]).apply_with_predicate(
                    &self.token,
                    stack,
                    |subs| -> Result<(), ApplicationError<'_>> {
                        match subs.get(&var) {
                            Some(Ty::U32) => Ok(()),
                            Some(Ty::EnumInstance(_, _)) => Ok(()),
                            Some(Ty::Enum(_)) => Ok(()),
                            Some(ty) => Err(ApplicationError::Message(
                                self.token.clone(),
                                format!("Don't know how to compare {ty:?}"),
                            )),
                            None => Err(ApplicationError::Other),
                        }
                    },
                )?
            }
            ExprKind::If(IfExpr { then, otherwise }) => {
                let (stack, subs) = FnTy::new([Ty::Bool], []).apply(&self.token, stack)?;

                let (then_stack, then_sub) = then.apply(stack.clone(), types, context, gen)?;
                let (otherwise_stack, otherwise_sub) = if let Some(otherwise) = otherwise {
                    otherwise.apply(stack.clone(), types, context, gen)?
                } else {
                    (stack.clone(), Substitution::new())
                };

                let subs = subs
                    .unify(then_sub)
                    .map_err(|e| ApplicationError::UnificationError(self.token.clone(), e))?
                    .unify(otherwise_sub)
                    .map_err(|e| ApplicationError::UnificationError(self.token.clone(), e))?;

                let then_stack = then_stack.substitute(&subs);
                let otherwise_stack = otherwise_stack.substitute(&subs);

                let subs = subs
                    .unify(
                        then_stack
                            .unify(otherwise_stack.clone(), Variance::Covariant)
                            .map_err(|e| {
                                ApplicationError::UnificationError(self.token.clone(), e)
                            })?,
                    )
                    .map_err(|e| ApplicationError::UnificationError(self.token, e))?;
                (otherwise_stack.substitute(&subs), subs)
            }
            ExprKind::TyInstance(ty_instance) => ty_instance.apply(&self.token, stack, types)?,
            ExprKind::DotSequence(seq) => unreachable!("{}", self.token.quote),
        };

        Ok((stack, subs))
    }

    pub fn resolve_names(
        &mut self,
        types: &Types<'src>,
        context: &Context<'src>,
    ) -> Result<(), ()> {
        match &mut self.kind {
            ExprKind::DotSequence(seq) => {
                seq.reverse();

                let top = seq
                    .pop()
                    .expect("Dot Sequences must have at least one element");

                match top.kind {
                    ExprKind::Var(VarExpr(top_ident)) => match types.get(top_ident) {
                        Some(Ty::Enum(EnumType {
                            variants,
                            ident: base,
                        })) => {
                            if seq.len() != 1 {
                                return Err(());
                            }

                            let next_id = seq.pop().unwrap();

                            match next_id.kind {
                                ExprKind::Var(VarExpr(ident)) => {
                                    if let Some(variant) =
                                        variants.iter().find(|variant| **variant == ident)
                                    {
                                        self.kind = ExprKind::TyInstance(TyInstanceExpr::Enum {
                                            base,
                                            variant: *variant,
                                        });
                                    } else if let Some(f) = context
                                        .iter()
                                        .find(|f| f.ident == self.token.quote().as_str())
                                    {
                                        self.kind =
                                            ExprKind::Var(VarExpr(self.token.quote.as_str()))
                                    } else {
                                        todo!("{ident}")
                                    }
                                }
                                _ => unreachable!("This shouldn't be reachable here..."),
                            }
                        }
                        Some(ty) => todo!("Not sure yet what to do with {:?}", ty),
                        None => todo!(),
                    },
                    kind => {
                        unreachable!("Top level kind is {kind:?}. This shouldn't be possible...")
                    }
                }
            }
            ExprKind::Block(BlockExpr(exprs)) => {
                for e in exprs {
                    e.resolve_names(types, context).unwrap();
                }
            }
            _ => (),
        }

        Ok(())
    }
}

impl<'src> From<BlockExpr<'src>> for ExprKind<'src> {
    fn from(value: BlockExpr<'src>) -> Self {
        Self::Block(value)
    }
}

impl<'src> From<LiteralExpr<'src>> for ExprKind<'src> {
    fn from(value: LiteralExpr<'src>) -> Self {
        Self::Literal(value)
    }
}

impl<'src> From<VarExpr<'src>> for ExprKind<'src> {
    fn from(value: VarExpr<'src>) -> Self {
        Self::Var(value)
    }
}

impl<'src> From<AsExpr<'src>> for ExprKind<'src> {
    fn from(value: AsExpr<'src>) -> Self {
        Self::As(value)
    }
}

impl<'src> From<IfExpr<'src>> for ExprKind<'src> {
    fn from(value: IfExpr<'src>) -> Self {
        Self::If(value)
    }
}

impl<'src> From<Literal<'src>> for ExprKind<'src> {
    fn from(value: Literal<'src>) -> Self {
        match value {
            Literal::U32(n) => ExprKind::Literal(n.into()),
            Literal::Bool(b) => ExprKind::Literal(b.into()),
            Literal::String(s) => ExprKind::Literal(s.into()),
        }
    }
}

use std::collections::HashMap;

use crate::{expr::Expr, lex::LexExtension, stmt::Function, types::Var};

impl<'src, M> Expr<'src, M, LexExtension<'src>> {
    pub fn resolve_names(
        self,
        fn_idents: &HashMap<String, usize>,
        extern_idents: &HashMap<String, usize>,
    ) -> Expr<'src, M, ()> {
        self.resolve_idents(fn_idents, extern_idents, &mut vec![])
    }

    fn resolve_idents(
        self,
        fn_idents: &HashMap<String, usize>,
        extern_idents: &HashMap<String, usize>,
        bound: &mut Vec<&'src str>,
    ) -> Expr<'src, M, ()> {
        match self.expr {
            crate::expr::ExprBase::Literal(lit) => Expr::literal(lit, self.meta),
            crate::expr::ExprBase::Print => Expr::print(self.meta),
            crate::expr::ExprBase::PrintString => Expr::print_string(self.meta),
            crate::expr::ExprBase::Block(exprs) => {
                let mut inner_bound = bound.clone();
                Expr::block(
                    exprs
                        .into_iter()
                        .map(|e| e.resolve_idents(fn_idents, extern_idents, &mut inner_bound))
                        .collect::<Vec<_>>(),
                    self.meta,
                )
            }
            crate::expr::ExprBase::While { condition, body } => {
                // We can allow for vars to be made in the condition.
                // These should _not_ be visilbe outside of the condition

                let mut bound_in_condition = bound.clone();
                Expr::wile(
                    condition
                        .into_iter()
                        .map(|e| {
                            e.resolve_idents(fn_idents, extern_idents, &mut bound_in_condition)
                        })
                        .collect::<Vec<_>>(),
                    body.resolve_idents(fn_idents, extern_idents, bound),
                    self.meta,
                )
            }
            crate::expr::ExprBase::BinOp(op) => Expr::binop(op, self.meta),
            crate::expr::ExprBase::Call(_) => todo!(),
            crate::expr::ExprBase::If { then, otherwise } => Expr::iff(
                then.resolve_idents(fn_idents, extern_idents, bound),
                otherwise.resolve_idents(fn_idents, extern_idents, bound),
                self.meta,
            ),

            crate::expr::ExprBase::Ext(LexExtension::Ident(ident)) => {
                if let Some(id) = fn_idents.get(ident) {
                    Expr::call(Var::Func(*id), self.meta)
                } else if let Some(id) = extern_idents.get(ident) {
                    Expr::xtern(*id, self.meta)
                } else {
                    let (idx, _) = bound
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, id)| **id == ident)
                        .expect("todo!(error prop)");
                    Expr::var(Var::Ident(idx), self.meta)
                }
            }
            crate::expr::ExprBase::Ext(LexExtension::As(idents)) => {
                let n = idents.len();
                bound.extend(idents);

                Expr::bind(n, self.meta)
            }
            crate::expr::ExprBase::ExternCall(id) => todo!(),
            crate::expr::ExprBase::Bind(_) => {
                panic!("Bind Expressions shouldn't be used before identifier resolution")
            }
            crate::expr::ExprBase::Var(_) => {
                panic!("Var Expressions shouldn't be used before identifier resolution")
            }
        }
    }
}

impl<'src, M> Function<'src, M, LexExtension<'src>> {
    pub fn resolve_names(
        self,
        fn_idents: &HashMap<String, usize>,
        extern_idents: &HashMap<String, usize>,
    ) -> Function<'src, M, ()> {
        Function {
            name: self.name,
            input: self.input,
            output: self.output,
            body: self.body.resolve_names(fn_idents, extern_idents),
            type_vars: self.type_vars,
        }
    }
}

use std::{collections::HashMap, fmt::Debug};

use crate::{
    c::{CType, CVar},
    expr::{BinOp, Expr, ExprBase},
    types::{Stack, Type, Var},
};

pub enum CSsaExtension<'src> {
    BackAssign { input: CVar, output: CVar },
    ExitLoop,
    Return(CType),
    Call(&'src str),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub input: Option<Vec<CVar>>,
    pub output: Option<Vec<CVar>>,
}

impl<'src, M, E> Expr<'src, M, E> {
    pub fn into_ssa_form(
        self,
        stack: Stack,
        env: &HashMap<Var, Type>,
        extern_fns: &'src HashMap<usize, CType>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Expr<'src, Assignment, CSsaExtension<'src>> {
        let (mut stack, mut counter) = CVar::from_stack(stack);
        self.get_var_ids(
            &mut stack,
            &mut counter,
            env,
            extern_fns,
            fn_names,
            &mut HashMap::new(),
        )
    }

    fn get_var_ids(
        self,
        stack: &mut Vec<CVar>,
        counter: &mut usize,
        env: &HashMap<Var, Type>,
        extern_fns: &'src HashMap<usize, CType>,
        fn_names: &'src HashMap<usize, String>,
        bound: &mut HashMap<usize, CVar>,
    ) -> Expr<'src, Assignment, CSsaExtension<'src>> {
        match self.expr {
            crate::expr::ExprBase::Literal(lit) => {
                let var: CVar = CVar::new(lit.clone().into(), counter);
                stack.push(var.clone());
                Expr::literal(
                    lit,
                    Assignment {
                        input: None,
                        output: Some(vec![var]),
                    },
                )
            }
            crate::expr::ExprBase::Block(exprs) => {
                let mut exprs: Vec<_> = exprs
                    .into_iter()
                    .map(|e| e.get_var_ids(stack, counter, env, extern_fns, fn_names, bound))
                    .collect();

                // we need to be able to "return" from a block, this means that we may need to
                // create an variable _before_ the block, and then back-assign at the end of
                // the block to assign it back.
                //
                // _If_ we have no items on the stack, then no work needs to be done.
                // _If_ we have _multiple_ items on the stack, then we need to do multiple
                // back-assignments
                let assignment = match stack.len() {
                    0 => Assignment {
                        input: None,
                        output: None,
                    },
                    _ => Assignment {
                        input: None,
                        output: Some(
                            stack
                                .iter_mut()
                                .map(|var| {
                                    let out_var = CVar::new(var.ty.clone(), counter);
                                    exprs.push(Expr::ext(
                                        CSsaExtension::BackAssign {
                                            input: var.clone(),
                                            output: out_var.clone(),
                                        },
                                        Assignment {
                                            input: None,
                                            output: None,
                                        },
                                    ));
                                    *var = out_var.clone();
                                    out_var
                                })
                                .collect(),
                        ),
                    },
                };

                Expr::block(exprs, assignment)
            }
            crate::expr::ExprBase::BinOp(op) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let var: CVar = CVar::new(CType::U32, counter);
                    stack.push(var.clone());
                    Expr::binop(
                        op,
                        Assignment {
                            input: Some(vec![left, right]),
                            output: Some(vec![var]),
                        },
                    )
                }
                BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt | BinOp::Ne => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let var: CVar = CVar::new(CType::Bool, counter);
                    stack.push(var.clone());
                    Expr::binop(
                        op,
                        Assignment {
                            input: Some(vec![left, right]),
                            output: Some(vec![var]),
                        },
                    )
                }
            },
            crate::expr::ExprBase::Call(f) => {
                let func = env
                    .get(&f)
                    .expect("call vars should be resolveable")
                    .clone()
                    .expect_function();

                let (input, output) = (func.input, func.output);

                let input = stack.split_off(stack.len() - input.len());

                // Since calling a function _can_ return multiple values,
                // things get a little tricky C-functions can only return a
                // single value, so we need to operate within that limitation.
                //
                // To get around this, functions which return multiple values
                // need to return a tuple instead, _then_ we'll back assign
                // values to destructure the tuple.

                // First, we need to generate the Vars, that will end up on the
                // stack after the function call.
                let out_vars = output
                    .into_iter()
                    .map(|ty| CVar::new(CType::from(ty), counter))
                    .collect::<Vec<_>>();

                // Then we push those vars onto the stack since they'll be
                // exposed
                stack.extend(out_vars.clone());

                // Then we need to generate the var which will be returned from
                // the function call, and create back-assignments
                let mut back_assignments = vec![];

                let call_assignment = if out_vars.len() > 0 {
                    let var = CVar::new(CType::from(out_vars.clone()), counter);

                    match &var.ty {
                        CType::Tuple(ts) => {
                            assert!(ts.len() == out_vars.len());

                            ts.iter().enumerate().zip(out_vars.iter()).for_each(
                                |((idx, _), output)| {
                                    back_assignments.push(Expr::ext(
                                        CSsaExtension::BackAssign {
                                            input: var.tuple_member(idx),
                                            output: output.clone(),
                                        },
                                        Assignment {
                                            input: None,
                                            output: None,
                                        },
                                    ))
                                },
                            );
                        }
                        _ => {
                            assert!(out_vars.len() == 1);

                            back_assignments.push(Expr::ext(
                                CSsaExtension::BackAssign {
                                    input: var.clone(),
                                    output: out_vars[0].clone(),
                                },
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ));
                        }
                    }
                    Assignment {
                        input: Some(input),
                        output: Some(vec![var]),
                    }
                } else {
                    Assignment {
                        input: Some(input),
                        output: None,
                    }
                };

                let fn_name = fn_names
                    .get(f.func())
                    .expect("Function names should be known");

                let mut block = vec![Expr::ext(CSsaExtension::Call(fn_name), call_assignment)];

                block.extend(back_assignments);

                Expr::block(
                    block,
                    Assignment {
                        input: None,
                        output: Some(out_vars),
                    },
                )
            }

            crate::expr::ExprBase::ExternCall(id) => {
                let extern_fn = extern_fns
                    .get(&id)
                    .expect("call vars should be resolveable")
                    .expect_function();

                let input = extern_fn.take_inputs_from_stack(stack);

                let output = match &*extern_fn.output {
                    CType::Variadic => unreachable!(),
                    CType::Void => None,
                    ty => {
                        let var = CVar::new(ty.clone(), counter);
                        stack.push(var.clone());
                        Some(vec![var])
                    }
                };

                Expr::ext(
                    CSsaExtension::Call(&extern_fn.name),
                    Assignment {
                        input: Some(input),
                        output,
                    },
                )
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                let condition = stack.pop().unwrap();
                assert!(condition.ty == CType::Bool);
                let mut then_stack = stack.clone();
                let mut otherwise_stack = stack.clone();
                let then =
                    then.get_var_ids(&mut then_stack, counter, env, extern_fns, fn_names, bound);
                let otherwise = otherwise.get_var_ids(
                    &mut otherwise_stack,
                    counter,
                    env,
                    extern_fns,
                    fn_names,
                    bound,
                );

                // collect the outputs into a vector (if any)
                match (&then.meta.output, &otherwise.meta.output) {
                    (None, None) => todo!(),
                    (None, Some(_)) | (Some(_), None) => {
                        unreachable!("If branches should product the same output")
                    }
                    (Some(then_out), Some(otherwise_out)) => {
                        assert!(then_out.len() == otherwise_out.len());

                        let mut output = vec![];
                        let mut then_back_assign = vec![];
                        let mut otherwise_back_assign = vec![];

                        then_out
                            .iter()
                            .zip(otherwise_out.iter())
                            .for_each(|(then, otherwise)| {
                                assert!(then.ty == otherwise.ty);

                                let out = CVar::new(then.ty.clone(), counter);
                                then_back_assign.push(Expr::ext(
                                    CSsaExtension::BackAssign {
                                        input: then.clone(),
                                        output: out.clone(),
                                    },
                                    Assignment {
                                        input: None,
                                        output: None,
                                    },
                                ));
                                otherwise_back_assign.push(Expr::ext(
                                    CSsaExtension::BackAssign {
                                        input: otherwise.clone(),
                                        output: out.clone(),
                                    },
                                    Assignment {
                                        input: None,
                                        output: None,
                                    },
                                ));
                                output.push(out);
                            });

                        let mut then_block = vec![then];
                        let mut otherwise_block = vec![otherwise];
                        then_block.extend(then_back_assign);
                        otherwise_block.extend(otherwise_back_assign);

                        stack.extend(output.clone());

                        Expr::iff(
                            Expr::block(
                                then_block,
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ),
                            Expr::block(
                                otherwise_block,
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ),
                            Assignment {
                                input: Some(vec![condition]),
                                output: Some(output),
                            },
                        )
                    }
                }
            }
            crate::expr::ExprBase::While { condition, body } => {
                let before = stack.clone();

                let mut condition: Vec<_> = condition
                    .into_iter()
                    .map(|e| e.get_var_ids(stack, counter, env, extern_fns, fn_names, bound))
                    .collect();

                let var = stack.pop().unwrap();

                condition.push(Expr::ext(
                    CSsaExtension::ExitLoop,
                    Assignment {
                        input: Some(vec![var]),
                        output: None,
                    },
                ));

                let mut body = body.get_var_ids(stack, counter, env, extern_fns, fn_names, bound);

                assert!(before.len() == stack.len());
                match &mut body.expr {
                    ExprBase::Block(ref mut exprs) => {
                        before.iter().zip(stack.iter()).for_each(|(before, after)| {
                            assert!(before.ty == after.ty);
                            exprs.push(Expr::ext(
                                CSsaExtension::BackAssign {
                                    input: after.clone(),
                                    output: before.clone(),
                                },
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ));
                        });
                    }
                    _ => todo!(),
                };

                *stack = before;

                Expr::wile(
                    condition,
                    body,
                    Assignment {
                        input: None,
                        output: None,
                    },
                )
            }
            crate::expr::ExprBase::Bind(n) => {
                assert!(stack.len() <= n);

                let popped = stack.split_off(stack.len() - n);
                popped.into_iter().for_each(|var| {
                    let id = bound.len();
                    bound.insert(id, var);
                });

                Expr::bind(
                    n,
                    Assignment {
                        input: None,
                        output: None,
                    },
                )
            }
            crate::expr::ExprBase::Var(Var::Ident(id)) => {
                let ref_var = bound.get(&id).expect("Bindings should be known");
                let var: CVar = CVar::new(ref_var.ty.clone(), counter);
                stack.push(var.clone());
                Expr::var(
                    Var::Ident(id),
                    Assignment {
                        input: Some(vec![ref_var.clone()]),
                        output: Some(vec![var]),
                    },
                )
            }

            crate::expr::ExprBase::VarAccessor(Var::Ident(id), accessors) => {
                let ref_var = bound.get(&id).expect("Bindings should be known");
                let var: CVar = CVar::new(
                    ref_var.ty.access_inner(&accessors).unwrap().clone(),
                    counter,
                );
                stack.push(var.clone());
                Expr::accessors(
                    Var::Ident(id),
                    accessors,
                    Assignment {
                        input: Some(vec![ref_var.clone()]),
                        output: Some(vec![var]),
                    },
                )
            }
            crate::expr::ExprBase::Var(Var::Func(_)) => unreachable!(),
            crate::expr::ExprBase::VarAccessor(Var::Func(_), ..) => unreachable!(),
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}

use crate::{
    ast::{
        expr::{TypedExpr, TypedOperatorExpr, TypedReadExpr},
        stmt::{Interfaces, StmtKind, UserDefinedTypes},
    },
    error::HayError,
    lex::token::{Operator, Token},
    types::{Frame, FreeVars, FunctionType, PointerType, Stack, Substitutions, Type, TypeVar},
};

#[derive(Debug, Clone)]
pub struct OperatorExpr {
    pub op: Operator,
    pub token: Token,
}

impl OperatorExpr {
    fn type_check_interface_op<S1: Into<String>, S2: Into<String>>(
        &self,
        stack: &mut Stack,
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
        interfaces: &Interfaces,
        interface_id: S1,
        interface_fn_name: S2,
        default_func: FunctionType,
    ) -> Result<TypedExpr, HayError> {
        let stack_before = stack.clone();
        if default_func.unify(&self.token, stack).is_err() {
            *stack = stack_before;

            let iface = interfaces.get(&interface_id.into()).unwrap();
            iface.unify(
                &self.token,
                stack,
                user_defined_types,
                free_vars,
                interfaces,
                &interface_fn_name.into(),
            )?;
        }
        Ok(TypedExpr::Operator(TypedOperatorExpr {
            op: self.op.clone(),
        }))
    }

    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &mut Frame,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
    ) -> Result<TypedExpr, HayError> {
        match &self.op {
            Operator::Minus => self.type_check_interface_op(
                stack,
                user_defined_types,
                None,
                interfaces,
                "Sub",
                "Op.sub",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            ),
            Operator::Plus => self.type_check_interface_op(
                stack,
                user_defined_types,
                None,
                interfaces,
                "Add",
                "Op.add",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            ),
            Operator::Star => self.type_check_interface_op(
                stack,
                user_defined_types,
                None,
                interfaces,
                "Mul",
                "Op.mul",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            ),
            Operator::Slash => self.type_check_interface_op(
                stack,
                user_defined_types,
                None,
                interfaces,
                "Div",
                "Op.div",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            ),
            Operator::GreaterEqual
            | Operator::LessThan
            | Operator::GreaterThan
            | Operator::LessEqual => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                FunctionType::unify_many(&fs, &self.token, stack)?;
                Ok(TypedExpr::Operator(TypedOperatorExpr {
                    op: self.op.clone(),
                }))
            }
            Operator::Modulo => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::u8()]),
                ];
                FunctionType::unify_many(&fs, &self.token, stack)?;
                todo!()
            }
            Operator::Equal | Operator::BangEqual => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::bool(), Type::bool()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                FunctionType::unify_many(&fs, &self.token, stack)?;
                todo!()
                // Signature::evaluate_many(
                //     &vec![
                //         // u64 == u64 -> bool
                //         Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::Bool.id()]),
                //         // u8 == u8   -> bool
                //         Signature::new(vec![Type::U8.id(), Type::U8.id()], vec![Type::Bool.id()]),
                //         // bool == bool -> bool
                //         Signature::new(
                //             vec![Type::Bool.id(), Type::Bool.id()],
                //             vec![Type::Bool.id()],
                //         ),
                //         // char == char -> char
                //         Signature::new(
                //             vec![Type::Char.id(), Type::Char.id()],
                //             vec![Type::Bool.id()],
                //         ),
                //         // *T == *T   -> bool
                //         Signature::new_generic(
                //             vec![TypeId::new("*T"), TypeId::new("*T")],
                //             vec![Type::Bool.id()],
                //             vec![TypeId::new("T")],
                //         ),
                //         // &T == &T   -> bool
                //         Signature::new_generic(
                //             vec![TypeId::new("&T"), TypeId::new("&T")],
                //             vec![Type::Bool.id()],
                //             vec![TypeId::new("T")],
                //         ),
                //         Signature::new_generic(
                //             vec![TypeId::new("E"), TypeId::new("E")],
                //             vec![Type::Bool.id()],
                //             vec![TypeId::new("E")],
                //         )
                //         .with_predicate(
                //             &|inputs, types| match (
                //                 types.get(&inputs[0].supertype(types)),
                //                 types.get(&inputs[1].supertype(types)),
                //             ) {
                //                 (
                //                     Some(Type::Enum { name: left, .. }),
                //                     Some(Type::Enum { name: right, .. }),
                //                 ) => left.lexeme == right.lexeme,
                //                 _ => false,
                //             },
                //             "E is an enum",
                //         ),
                //     ],
                //     &self.token,
                //     stack,
                //     types,
                //     Variance::Covariant,
                // )?;
            }
            Operator::Read => {
                let t = Type::TypeVar(TypeVar::new("T"));
                let ptr = Type::Pointer(PointerType {
                    mutable: false,
                    inner: Box::new(t.clone()),
                });

                let func = FunctionType::new(vec![ptr], vec![t.clone()]);
                let subs = func.unify(&self.token, stack)?;

                let typ = t.substitute(&self.token, &subs)?;

                Ok(TypedExpr::Read(TypedReadExpr { typ }))
            }
            Operator::Write => {
                let t = Type::TypeVar(TypeVar::new("T"));
                let ptr = Type::Pointer(PointerType {
                    mutable: true,
                    inner: Box::new(t.clone()),
                });

                let func = FunctionType::new(vec![t, ptr], vec![]);
                func.unify(&self.token, stack)?;
                todo!()
            }
            op => todo!("{op:?}"),
        }
    }
}

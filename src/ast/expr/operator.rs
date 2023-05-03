use crate::{
    ast::stmt::{Interfaces, StmtKind, UserDefinedTypes},
    error::HayError,
    lex::token::{Operator, Token},
    types::{Frame, FunctionType, PointerType, Stack, Substitutions, Type, TypeVar},
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
        interfaces: &Interfaces,
        interface_id: S1,
        interface_fn_name: S2,
        default_func: FunctionType,
    ) -> Result<(), HayError> {
        if default_func.unify(&self.token, stack).is_err() {
            let iface = interfaces.get(&interface_id.into()).unwrap();
            iface.unify(&interface_fn_name.into(), &self.token, stack)?;
        }
        Ok(())
    }

    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &mut Frame,
        interfaces: &Interfaces,
    ) -> Result<(), HayError> {
        match &self.op {
            Operator::Minus => self.type_check_interface_op(
                stack,
                interfaces,
                "Sub",
                "Op.sub",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            )?,
            Operator::Plus => self.type_check_interface_op(
                stack,
                interfaces,
                "Add",
                "Op.add",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            )?,
            Operator::GreaterEqual | Operator::LessThan => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                FunctionType::unify_many(&fs, &self.token, stack)?;
            }

            Operator::Equal => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::bool(), Type::bool()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                println!("EQUALS.....");
                FunctionType::unify_many(&fs, &self.token, stack)?;
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

                let func = FunctionType::new(vec![ptr], vec![t]);
                func.unify(&self.token, stack)?;
            }
            Operator::Write => {
                let t = Type::TypeVar(TypeVar::new("T"));
                let ptr = Type::Pointer(PointerType {
                    mutable: true,
                    inner: Box::new(t.clone()),
                });

                let func = FunctionType::new(vec![t, ptr], vec![]);
                func.unify(&self.token, stack)?;
            }
            op => todo!("{op:?}"),
        }

        Ok(())
    }
}

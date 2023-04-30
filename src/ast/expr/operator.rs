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
        subs: &mut Substitutions,
        interfaces: &Interfaces,
        interface_id: S1,
        interface_fn_name: S2,
        default_func: FunctionType,
    ) -> Result<(), HayError> {
        if default_func
            .unify(&self.token, stack, &mut Substitutions::empty())
            .is_err()
        {
            let iface = interfaces.get(&interface_id.into()).unwrap();
            iface.unify(&interface_fn_name.into(), &self.token, stack, subs)?;
        }
        Ok(())
    }

    pub fn type_check(
        &self,
        types: &UserDefinedTypes,
        stack: &mut Stack,
        frame: &mut Frame,
        interfaces: &Interfaces,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        match &self.op {
            Operator::Minus => self.type_check_interface_op(
                stack,
                subs,
                interfaces,
                "Sub",
                "Op.sub",
                FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::u64()]),
            )?,
            Operator::GreaterEqual => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                FunctionType::unify_many(&fs, &self.token, stack, subs)?;
            }
            Operator::Read => {
                let t = Type::TypeVar(TypeVar::new("T"));
                let ptr = Type::Pointer(PointerType {
                    mutable: false,
                    inner: Box::new(t.clone()),
                });

                let func = FunctionType::new(vec![ptr], vec![t]);
                func.unify(&self.token, stack, &mut Substitutions::empty())?;
            }
            op => todo!("{op:?}"),
        }

        Ok(())
    }
}

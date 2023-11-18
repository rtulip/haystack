use crate::expression::{AsExpr, Expr, ExprKind, IfExpr, VarExpr};

pub use self::element::Element;

mod element;

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(String),
}

pub struct Interpreter<'src> {
    pub stack: Vec<Element<'src>>,
    context: Vec<(&'src str, Element<'src>)>,
}

impl<'src> Interpreter<'src> {
    pub fn new<T>(context: T) -> Self
    where
        T: IntoIterator<Item = (&'src str, Element<'src>)>,
    {
        Self {
            stack: vec![],
            context: context.into_iter().collect(),
        }
    }

    pub fn start(mut self, entry: &str) -> Result<(), InterpreterError> {
        let elem = self
            .context
            .iter()
            .find(|(id, _)| *id == entry)
            .map(|(_, e)| e)
            .unwrap();
        self.execute(elem.expr().clone())
    }

    fn execute(&mut self, expr: Expr<'src>) -> Result<(), InterpreterError> {
        match expr.kind {
            ExprKind::Literal(lit) => self.stack.push(lit.into()),
            ExprKind::Block(exprs) => {
                let len = self.context.len();
                for expr in exprs {
                    self.execute(expr)?
                }
                self.context.truncate(len);
            }
            ExprKind::Var(VarExpr(var)) => {
                match self
                    .context
                    .iter()
                    .rev()
                    .find(|(id, _)| *id == var)
                    .ok_or(InterpreterError::RuntimeError(format!(
                        "Unknown var: {var}"
                    )))?
                    .1
                    .clone()
                {
                    Element::Bool(b) => self.stack.push(b.into()),
                    Element::U32(n) => self.stack.push(n.into()),
                    Element::Str(s) => self.stack.push(s.into()),
                    Element::Expr(e) => self.execute(e)?,
                    Element::Extern(f) => f(self)?,
                }
            }
            ExprKind::As(AsExpr(bindings)) => {
                if bindings.len() > self.stack.len() {
                    return Err(InterpreterError::RuntimeError(
                        "Too few elements on the stack".into(),
                    ));
                } else {
                    let tail = self.stack.split_off(self.stack.len() - bindings.len());
                    for (id, elem) in bindings.into_iter().zip(tail.into_iter()) {
                        self.context.push((id, elem))
                    }
                }
            }
            ExprKind::Add(_) => {
                let l = self.pop_u32()?;
                let r = self.pop_u32()?;
                self.stack.push((l + r).into())
            }
            ExprKind::Sub(_) => {
                let b = self.pop_u32()?;
                let a = self.pop_u32()?;
                self.stack.push((a - b).into())
            }
            ExprKind::LessThan(_) => {
                let b = self.pop_u32()?;
                let a = self.pop_u32()?;
                self.stack.push((a < b).into())
            }
            ExprKind::If(IfExpr { then, otherwise }) => {
                if self.pop_bool()? {
                    self.execute(*then)?;
                } else {
                    if let Some(otherwise) = otherwise {
                        self.execute(*otherwise)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn pop_u32(&mut self) -> Result<u32, InterpreterError> {
        if let Some(Element::U32(n)) = self.stack.pop() {
            Ok(n)
        } else {
            Err(InterpreterError::RuntimeError("Expected a u32".into()))
        }
    }

    pub fn pop_str(&mut self) -> Result<&'src str, InterpreterError> {
        if let Some(Element::Str(n)) = self.stack.pop() {
            Ok(n)
        } else {
            Err(InterpreterError::RuntimeError("Expected a Str".into()))
        }
    }

    pub fn pop_bool(&mut self) -> Result<bool, InterpreterError> {
        if let Some(Element::Bool(b)) = self.stack.pop() {
            Ok(b)
        } else {
            Err(InterpreterError::RuntimeError("Expected a Str".into()))
        }
    }
}

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
    fn as_let_statement(&self) -> Option<&LetStatement> {
        None
    }
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Box<Identifier>,
    pub value: Box<dyn Expression>,
}

impl<'a> Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl<'a> Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_let_statement(&self) -> Option<&LetStatement> {
        Some(self)
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

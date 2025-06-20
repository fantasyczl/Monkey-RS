use crate::token::Token;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
    fn as_let_statement(&self) -> Option<&LetStatement> {
        None
    }

    fn as_return_statement(&self) -> Option<&ReturnStatement> {
        None
    }

    fn as_expression_statement(&self) -> Option<&ExpressionStatement> {
        None
    }
}

pub trait Expression: Node + std::fmt::Debug {
    fn expression_node(&self);

    fn as_identifier(&self) -> Option<&Identifier> {
        None
    }

    fn as_integer_literal(&self) -> Option<&IntegerLiteral> {
        None
    }
    
    fn as_prefix_expression(&self) -> Option<&PrefixExpression> {
        None
    }
    
    fn as_infix_expression(&self) -> Option<&InfixExpression> {
        None
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }

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

#[derive(Debug)]
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
    fn as_identifier(&self) -> Option<&Identifier> {
        Some(self)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
    fn as_expression_statement(&self) -> Option<&ExpressionStatement> {
        Some(self)
    }
}

#[derive(Debug)]
pub struct IntegerLiteral{
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
    fn as_integer_literal(&self) -> Option<&IntegerLiteral> {
        Some(self)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
    
    fn as_prefix_expression(&self) -> Option<&PrefixExpression> {
        Some(self)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO
        // write!(f, "({} {} {})", self.left, self.operator, self.right)
        write!(f, "($left {} $right)", self.operator)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
    
    fn as_infix_expression(&self) -> Option<&InfixExpression> {
        Some(self)
    }
}

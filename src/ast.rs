use std::any::Any;
use crate::token::{Token};
use std::fmt;
use std::fmt::{Display, Formatter};

pub trait Node: Any {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;

    // optional methods for downcasting
    fn as_program(&self) -> Option<&Program> {
        None
    }
}

pub trait Statement: Node + fmt::Display + fmt::Debug {
    fn as_let_statement(&self) -> Option<&LetStatement> {
        None
    }
    fn as_return_statement(&self) -> Option<&ReturnStatement> {
        None
    }
    fn as_expression_statement(&self) -> Option<&ExpressionStatement> {
        None
    }
    fn clone_box(&self) -> Box<dyn Statement>;
}

pub trait Expression: Node + fmt::Debug + fmt::Display {
    fn as_identifier(&self) -> Option<&Identifier> {
        None
    }
    fn as_integer_literal(&self) -> Option<&IntegerLiteral> {
        None
    }
    fn as_string_literal(&self) -> Option<&StringLiteral> {
        None
    }
    fn as_prefix_expression(&self) -> Option<&PrefixExpression> {
        None
    }
    fn as_infix_expression(&self) -> Option<&InfixExpression> {
        None
    }
    fn as_boolean(&self) -> Option<&Boolean> {
        None
    }
    fn as_if_expression(&self) -> Option<&IfExpression> {
        None
    }
    fn as_function_literal(&self) -> Option<&FunctionLiteral> {
        None
    }
    fn as_call_expression(&self) -> Option<&CallExpression> {
        None
    }

    fn clone_box(&self) -> Box<dyn Expression>;
    fn as_array_literal(&self) -> Option<&ArrayLiteral> {None}
    fn as_index_expression(&self) -> Option<&IndexExpression> {None}
    fn as_hash_literal(&self) -> Option<&HashLiteral> {None}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        "Program".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_program(&self) -> Option<&Program> {
        Some(self)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        write!(f, "{}", out)
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Box<Identifier>,
    pub value: Box<dyn Expression>,
}

impl<'a> Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<'a> Statement for LetStatement {
    fn as_let_statement(&self) -> Option<&LetStatement> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Statement> {
        let let_statement = LetStatement {
            token: self.token.clone(),
            name: self.name.clone(),
            value: self.value.clone_box(),
        };
        Box::new(let_statement)
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name.to_string(),
            self.value.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn default() -> Self {
        Identifier {
            token: Token::new_illegal(),
            value: "".to_string(),
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn as_identifier(&self) -> Option<&Identifier> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn as_return_statement(&self) -> Option<&ReturnStatement> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Statement> {
        let ret = ReturnStatement {
            token: self.token.clone(),
            return_value: self.return_value.as_ref().map(|v| v.clone_box()),
        };

        Box::new(ret)
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str(self.token_literal().as_str());

        if let Some(return_value) = &self.return_value {
            out.push_str(" ");
            out.push_str(&return_value.to_string());
        }

        write!( f, "{};", out)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {
    fn as_expression_statement(&self) -> Option<&ExpressionStatement> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Statement> {
        let expression_statement = ExpressionStatement {
            token: self.token.clone(),
            expression: self.expression.as_ref().map(|e| e.clone_box()),
        };
        Box::new(expression_statement)
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.expression {
            write!(f, "{}", expr.to_string())
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IntegerLiteral {
    fn as_integer_literal(&self) -> Option<&IntegerLiteral> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(IntegerLiteral {
            token: self.token.clone(),
            value: self.value,
        })
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Node for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for StringLiteral {
    fn as_string_literal(&self) -> Option<&StringLiteral> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(StringLiteral {
            token: self.token.clone(),
            value: self.value.clone(),
        })
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn as_prefix_expression(&self) -> Option<&PrefixExpression> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        let prefix_expression = PrefixExpression {
            token: self.token.clone(),
            operator: self.operator.clone(),
            right: self.right.as_ref().map(|r| r.clone_box()),
        };
        Box::new(prefix_expression)
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(right) = &self.right {
            write!(f, "({}{})", self.operator, right.to_string())
        } else {
            write!(f, "({}{})", self.operator, "")
        }
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
        write!(f, "({} {} {})", self.left.to_string(), self.operator, self.right.to_string())
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn as_infix_expression(&self) -> Option<&InfixExpression> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        let infix_expression = InfixExpression {
            token: self.token.clone(),
            left: self.left.clone_box(),
            operator: self.operator.clone(),
            right: self.right.clone_box(),
        };
        Box::new(infix_expression)
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Boolean {
    fn as_boolean(&self) -> Option<&Boolean> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(Boolean {
            token: self.token.clone(),
            value: self.value,
        })
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(tp: Token) -> Self {
        let mut exp = Self::default();
        exp.token = tp;
        exp
    }
    pub fn default() -> Self {
        IfExpression {
            token: Token::new_illegal(),
            condition: Box::new(Identifier::default()),
            consequence: BlockStatement::default(),
            alternative: None,
        }
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {
    fn as_if_expression(&self) -> Option<&IfExpression> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        let if_expression = IfExpression {
            token: self.token.clone(),
            condition: self.condition.clone_box(),
            consequence: self.consequence.clone(),
            alternative: self.alternative.as_ref().map(|a| a.clone()),
        };
        Box::new(if_expression)
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str("if ");
        out.push_str(&self.condition.to_string());
        out.push_str(" ");
        out.push_str(&self.consequence.to_string());
        if let Some(alternative) = &self.alternative {
            out.push_str(" else ");
            out.push_str(&alternative.to_string());
        }
        write!(f, "{}", out)
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: Token) -> Self {
        BlockStatement {
            token,
            statements: vec![],
        }
    }

    pub fn default() -> Self {
        BlockStatement {
            token: Token::new_illegal(),
            statements: vec![],
        }
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        write!(f, "{}", out)
    }
}

impl Clone for BlockStatement {
    fn clone(&self) -> Self {
        BlockStatement {
            token: self.token.clone(),
            statements: self.statements.iter()
                .map(|s| s.clone_box())
                .collect(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(token: Token) -> Self {
        FunctionLiteral {
            token,
            parameters: vec![],
            body: BlockStatement::default(),
        }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for FunctionLiteral {
    fn as_function_literal(&self) -> Option<&FunctionLiteral> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        let function_literal = FunctionLiteral {
            token: self.token.clone(),
            parameters: self.parameters.clone(),
            body: self.body.clone(),
        };
        Box::new(function_literal)
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str("(");
        for (i, param) in self.parameters.iter().enumerate() {
            out.push_str(&param.to_string());
            if i < self.parameters.len() - 1 {
                out.push_str(", ");
            }
        }
        out.push_str(") ");
        out.push_str(&self.body.to_string());
        write!(f, "{}", out)
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {
    fn as_call_expression(&self) -> Option<&CallExpression> {
        Some(self)
    }
    fn clone_box(&self) -> Box<dyn Expression> {
        let call_expression = CallExpression {
            token: self.token.clone(),
            function: self.function.clone_box(),
            arguments: self.arguments.iter()
                .map(|arg| arg.clone_box())
                .collect(),
        };
        Box::new(call_expression)
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str(&self.function.to_string());
        out.push_str("(");
        for (i, arg) in self.arguments.iter().enumerate() {
            out.push_str(&arg.to_string());
            if i < self.arguments.len() - 1 {
                out.push_str(", ");
            }
        }
        out.push_str(")");
        write!(f, "{}", out)
    }
}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<dyn Expression>>,
}

impl ArrayLiteral {
    pub fn new(token: Token) -> Self {
        ArrayLiteral {
            token,
            elements: vec![],
        }
    }
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str("[");
        for (i, element) in self.elements.iter().enumerate() {
            out.push_str(&element.to_string());
            if i < self.elements.len() - 1 {
                out.push_str(", ");
            }
        }
        out.push_str("]");
        write!(f, "{}", out)
    }
}

impl Expression for ArrayLiteral {
    fn clone_box(&self) -> Box<dyn Expression> {
        let array_literal = ArrayLiteral {
            token: self.token.clone(),
            elements: self.elements.iter()
                .map(|e| e.clone_box())
                .collect(),
        };
        Box::new(array_literal)
    }
    fn as_array_literal(&self) -> Option<&ArrayLiteral> {
        Some(self)
    }
}

#[derive(Debug)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}

impl IndexExpression {
    pub fn new(token: Token, left: Box<dyn Expression>) -> Self {
        IndexExpression {
            token,
            left,
            index: Box::new(Identifier::default()),
        }
    }

}

impl Node for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IndexExpression {
    fn clone_box(&self) -> Box<dyn Expression> {
        let index_expression = IndexExpression {
            token: self.token.clone(),
            left: self.left.clone_box(),
            index: self.index.clone_box(),
        };
        Box::new(index_expression)
    }
    fn as_index_expression(&self) -> Option<&IndexExpression> {
        Some(self)
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Debug)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Box<dyn Expression>, Box<dyn Expression>)>,
}

impl HashLiteral {
    pub fn new(token: Token) -> Self {
        HashLiteral {
            token,
            pairs: vec![],
        }
    }
}

impl Node for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str("{");
        for (i, (key, value)) in self.pairs.iter().enumerate() {
            out.push_str(&format!("{}: {}", key.to_string(), value.to_string()));
            if i < self.pairs.len() - 1 {
                out.push_str(", ");
            }
        }
        out.push_str("}");
        write!(f, "{}", out)
    }
}

impl Expression for HashLiteral {
    fn clone_box(&self) -> Box<dyn Expression> {
        let hash_literal = HashLiteral {
            token: self.token.clone(),
            pairs: self.pairs.iter()
                .map(|(k, v)| (k.clone_box(), v.clone_box()))
                .collect(),
        };
        Box::new(hash_literal)
    }
    fn as_hash_literal(&self) -> Option<&HashLiteral> {
        Some(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType::{IDENT, INT, LET, RETURN};

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token {
                        tp: LET,
                        literal: "let".to_string(),
                    },
                    name: Box::new(Identifier {
                        token: Token {
                            tp: IDENT,
                            literal: "myVar".to_string(),
                        },
                        value: "myVar".to_string(),
                    }),
                    value: Box::new(IntegerLiteral {
                        token: Token {
                            tp: INT,
                            literal: "5".to_string(),
                        },
                        value: 5,
                    }),
                }),
                Box::new(ReturnStatement {
                    token: Token {
                        tp: RETURN,
                        literal: "return".to_string(),
                    },
                    return_value: Some(Box::new(IntegerLiteral {
                        token: Token {
                            tp: INT,
                            literal: "10".to_string(),
                        },
                        value: 10,
                    })),
                }),
            ],
        };

        assert_eq!(program.to_string(), "let myVar = 5;return 10;");
    }
}

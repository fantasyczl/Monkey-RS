use crate::ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement, ExpressionStatement, IntegerLiteral};
use crate::lexer::Lexer;
use crate::token::TokenType::{ASSIGN, IDENT, INT, LET, SEMICOLON};
use crate::token::{Token, TokenType};

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(Box<dyn Expression>) -> Box<dyn Expression>;

const LOWEST: i32 = 0; // 最低优先级
const EQUALS: i32 = 1; // ==
const LESSGREATER: i32 = 2; // < or >
const SUM: i32 = 3; // + or -
const PRODUCT: i32 = 4; // * or /
const PREFIX: i32 = 5; // -X or !X
const CALL: i32 = 6; // myFunction(X)

pub struct Parser<'a> {
    l: &'a mut Lexer,

    errors: Vec<String>, // 用于存储解析错误

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: std::collections::HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: std::collections::HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer) -> Self {
        let mut p = Parser {
            l,
            errors: vec![],
            cur_token: Token::new(TokenType::ILLEGAL, ""),
            peek_token: Token::new(TokenType::ILLEGAL, ""),
            prefix_parse_fns: Default::default(),
            infix_parse_fns: Default::default(),
        };

        p.register_prefix(IDENT, parse_identifier);
        p.register_prefix(INT, parse_integer_literal);

        // 读取两个词法单元，以设置 cur_token 和 peek_token
        p.next_token();
        p.next_token();

        p
    }

    fn register_prefix(&mut self, tp: TokenType, fn_: PrefixParseFn) {
        self.prefix_parse_fns.insert(tp, fn_);
    }

    fn register_infix(&mut self, tp: TokenType, fn_: InfixParseFn) {
        self.infix_parse_fns.insert(tp, fn_);
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn add_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.tp
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Program {
        // 创建一个新的 Program 实例
        let mut program = Program::new();

        while self.cur_token.tp != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(statement) = stmt {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.tp {
            LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(), // 其他语句类型可以在这里添加
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = Box::new(LetStatement {
            token: self.cur_token.clone(),
            name: Box::new(Identifier {
                token: Token::new_illegal(),
                value: "".to_string(), // 初始值为空，稍后会被更新
            }),
            value: Box::new(Identifier {
                token: Token::new_illegal(),
                value: "".to_string(),
            }),
        });

        if !self.expect_peek(IDENT) {
            return None;
        }

        stmt.name = Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(ASSIGN) {
            return None;
        }

        // TODO: 跳过对表达式的处理，直到遇到分号
        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt = Box::new(ReturnStatement {
            token: self.cur_token.clone(),
            return_value: Box::new(Identifier {
                token: Token::new_illegal(),
                value: "".to_string(), // 初始值为空，稍后会被更新
            }),
        });

        self.next_token();

        // TODO: 跳过对表达式的处理，直到遇到分号
        while !self.cur_token_is(SEMICOLON) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = Box::new(ExpressionStatement {
            token: self.cur_token.clone(),
            expression: None,
        });

        stmt.expression = self.parse_expression(LOWEST);

        // 如果下一个 token 是分号，则跳过它
        if self.peek_token_is(SEMICOLON) {
            self.next_token(); // 跳过分号
        }

        Some(stmt)
    }

    fn parse_expression(&mut self, _precedence: i32) -> Option<Box<dyn Expression>> {
        let prefix  = self.prefix_parse_fns.get(&self.cur_token.tp);
        match prefix {
            Some(&fn_) => fn_(self),
            None => None,
        }
    }

    fn cur_token_is(&self, tp: TokenType) -> bool {
        self.cur_token.tp == tp
    }

    fn peek_token_is(&self, tp: TokenType) -> bool {
        self.peek_token.tp == tp
    }

    fn expect_peek(&mut self, tp: TokenType) -> bool {
        if self.peek_token_is(tp) {
            self.next_token();
            true
        } else {
            self.peek_error(tp);
            false
        }
    }
}

fn parse_identifier(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    Some(Box::new(Identifier {
        token: parser.cur_token.clone(),
        value: parser.cur_token.literal.clone(),
    }))
}

fn parse_integer_literal(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    let result = parser.cur_token.literal.parse::<i64>();

    match result {
        Ok(val) => {
            let literal = Box::new(IntegerLiteral {
                token: parser.cur_token.clone(),
                value: val,
            });
            Some(literal)
        },
        Err(e) => {
            parser.add_error(format!( "无法解析整数字面量: {}", e ));
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use crate::ast::Statement;

    #[test]
    fn test_let_statements() {
        // 这里可以添加测试代码来验证 let 语句的解析
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_ne!(program.statements.len(), 0);
        assert_eq!(program.statements.len(), 3);

        let tests = vec![("x", 5), ("y", 10), ("foobar", 838383)];

        for (i, tt) in tests.iter().enumerate() {
            if let Some(stmt) = program.statements.get(i) {
                test_let_statement(stmt, tt)
            } else {
                assert!(false, "no statement at index {}", i);
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors:", errors.len());
        for error in errors {
            eprintln!("  {}", error);
        }

        panic!("parser has {} errors", errors.len());
    }

    fn test_let_statement(stmt: &Box<dyn Statement>, tt: &(&str, i32)) {
        assert_eq!(stmt.token_literal(), "let");
        if let Some(let_stmt) = stmt.as_let_statement() {
            assert_eq!(let_stmt.name.value, tt.0);
        } else {
            panic!("stmt is not LetStatement");
        }
    }

    #[test]
    fn test_return_statements() {
        // 这里可以添加测试代码来验证 return 语句的解析
        let input = r#"
        return 5;
return 10;
return 838383;
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            if let Some(return_stmt) = stmt.as_return_statement() {
                assert_eq!(return_stmt.token_literal(), "return");
                // 这里可以添加更多的断言来验证 return 语句的值
            } else {
                panic!("stmt is not ReturnStatement");
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = r#"
        foobar;
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        if let Some(stmt) = program.statements.get(0) {
            match stmt.as_expression_statement() {
                Some(expr_stmt) => {
                    match &expr_stmt.expression {
                        Some(expr) => {
                            if let Some(ident) = expr.as_identifier() {
                                assert_eq!(ident.value, "foobar");
                                assert_eq!(ident.token_literal(), "foobar");
                            } else {
                                panic!("expression is not Identifier");
                            }
                        },
                        None => panic!("expression is None"),
                    }
                },
                None => panic!("statement is not ExpressionStatement"),
            }
        } else {
            assert!(false, "no statement at index {}", 0);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = r#"
        5;
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0) {
            None => panic!("statement is not ExpressionStatement"),
            Some(stmt) => {
                match stmt.as_expression_statement() {
                    None => panic!("statement is not ExpressionStatement"),
                    Some(expr_stmt) => {
                        match &expr_stmt.expression {
                            None => panic!("expression is None"),
                            Some(expr) => {
                                if let Some(ident) = expr.as_integer_literal() {
                                    assert_eq!(ident.value, 5);
                                    assert_eq!(ident.token_literal(), "5");
                                } else {
                                    panic!("expression is not IdentifierLiteral");
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestCase {
            input: &'static str,
            operator: &'static str,
            value: i64,
        }

        let tests = vec![
            TestCase { input: "!5;", operator: "!", value: 5 },
            TestCase { input: "-15;", operator: "-", value: 15 },
        ];

        for test in tests {
            let mut l = Lexer::new(test.input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);

            match program.statements.get(0) {
                None => panic!("statement index 0 is None"),
                Some(stmt) => {
                    match stmt.as_expression_statement() {
                        None => panic!("statement is not ExpressionStatement"),
                        Some(expr_stmt) => {
                            match &expr_stmt.expression {
                                None => panic!("expression is None"),
                                Some(expr) => {
                                    if let Some(prefix_expr) = expr.as_prefix_expression() {
                                        assert_eq!(prefix_expr.operator, test.operator);
                                        test_integer_literal(&prefix_expr.right, test.value);
                                    } else {
                                        panic!("expression is not PrefixExpression");
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn test_integer_literal(right: &Box<dyn Expression>, value: i64) {

    }
}

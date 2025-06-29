use crate::ast::{Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement, InfixExpression, Boolean, BlockStatement, FunctionLiteral};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use once_cell::sync::Lazy;
use std::collections::HashMap;

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(Box<dyn Expression>, &mut Parser) -> Box<dyn Expression>;

const LOWEST: i32 = 0; // 最低优先级
const EQUALS: i32 = 1; // ==
const LESS_GREATER: i32 = 2; // < or >
const SUM: i32 = 3; // + or -
const PRODUCT: i32 = 4; // * or /
const PREFIX: i32 = 5; // -X or !X
const CALL: i32 = 6; // myFunction(X)

// 定义操作符的优先级
static PRECEDENCES: Lazy<HashMap<TokenType, i32>> = Lazy::new(|| {
    HashMap::from([
        (TokenType::EQ, EQUALS),
        (TokenType::NotEq, EQUALS),
        (TokenType::LT, LESS_GREATER),
        (TokenType::GT, LESS_GREATER),
        (TokenType::PLUS, SUM),
        (TokenType::MINUS, SUM),
        (TokenType::SLASH, PRODUCT),
        (TokenType::ASTERISK, PRODUCT),
        (TokenType::LPAREN, CALL),
    ])
});

pub struct Parser<'a> {
    l: &'a mut Lexer,

    errors: Vec<String>, // 用于存储解析错误

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
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

        p.register_prefix(TokenType::IDENT, parse_identifier);
        p.register_prefix(TokenType::INT, parse_integer_literal);
        p.register_prefix(TokenType::BANG, parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, parse_prefix_expression);
        p.register_prefix(TokenType::TRUE, parse_boolean);
        p.register_prefix(TokenType::FALSE, parse_boolean);
        p.register_prefix(TokenType::LPAREN, parse_grouped_expression);
        p.register_prefix(TokenType::IF, parse_if_expression);
        p.register_prefix(TokenType::FUNCTION, parse_function_literal);

        // 注册中缀解析函数
        p.register_infix(TokenType::PLUS, parse_infix_expression);
        p.register_infix(TokenType::MINUS, parse_infix_expression);
        p.register_infix(TokenType::SLASH, parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, parse_infix_expression);
        p.register_infix(TokenType::EQ, parse_infix_expression);
        p.register_infix(TokenType::NotEq, parse_infix_expression);
        p.register_infix(TokenType::LT, parse_infix_expression);
        p.register_infix(TokenType::GT, parse_infix_expression);
        p.register_infix(TokenType::LPAREN, parse_call_expression);

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
            TokenType::LET => self.parse_let_statement(),
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

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        stmt.name = Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        // TODO: 跳过对表达式的处理，直到遇到分号
        while !self.cur_token_is(TokenType::SEMICOLON) {
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
        while !self.cur_token_is(TokenType::SEMICOLON) {
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
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token(); // 跳过分号
        }

        Some(stmt)
    }

    fn no_prefix_parse_fn_error(&mut self, tp: TokenType) {
        let msg = format!("没有为 {:?} 注册前缀解析函数", tp);
        self.add_error(msg);
    }

    fn parse_expression(&mut self, _precedence: i32) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.tp).copied();
        match prefix {
            Some(fn_) => {
                let mut left_exp = fn_(self);
                while !self.peek_token_is(TokenType::SEMICOLON) && _precedence < self.peek_precedence() {
                    let infix = self.infix_parse_fns.get(&self.peek_token.tp).copied();
                    match infix {
                        None => {
                            break; // 如果没有中缀解析函数，直接返回左表达式
                        }
                        Some(infix_fn) => {
                            self.next_token(); // 移动到下一个 token
                            // 调用中缀解析函数
                            if let Some(left) = left_exp {
                                left_exp = Some(infix_fn(left, self))
                            }
                        }
                    }
                }

                left_exp
            },
            None => {
                // add error
                self.no_prefix_parse_fn_error(self.cur_token.tp);

                None
            }
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

    fn peek_precedence(&self) -> i32 {
        PRECEDENCES
            .get(&self.peek_token.tp)
            .cloned()
            .unwrap_or(LOWEST)
    }

    fn cur_precedence(&self) -> i32 {
        PRECEDENCES
            .get(&self.cur_token.tp)
            .cloned()
            .unwrap_or(LOWEST)
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
        }
        Err(e) => {
            parser.add_error(format!("无法解析整数字面量: {}", e));
            None
        }
    }
}

fn parse_prefix_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    // 这里可以实现前缀表达式的解析逻辑
    // 例如处理 !5 或 -15 等

    let expression = Box::new(PrefixExpression {
        token: parser.cur_token.clone(),
        operator: parser.cur_token.literal.clone(),
        right: None,
    });

    parser.next_token();

    parser.parse_expression(PREFIX).map(|right| {
        let mut prefix_expr = expression;
        prefix_expr.right = Some(right);
        prefix_expr as Box<dyn Expression>
    })
}

fn parse_boolean(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    let value = match parser.cur_token.tp {
        TokenType::TRUE => true,
        TokenType::FALSE => false,
        _ => return None, // 如果不是布尔类型，返回 None
    };

    Some(Box::new(Boolean {
        token: parser.cur_token.clone(),
        value,
    }))
}

fn parse_infix_expression(left: Box<dyn Expression>, parser: &mut Parser) -> Box<dyn Expression> {
    let mut expression = Box::new(InfixExpression {
        token: parser.cur_token.clone(),
        left,
        operator: parser.cur_token.literal.clone(),
        right: Box::new(Identifier {
            token: Token::new_illegal(),
            value: "".to_string(), // 初始值为空，稍后会被更新
        }),
    });

    let precedence = parser.cur_precedence();
    parser.next_token();

    if let Some(right) = parser.parse_expression(precedence) {
        expression.right = right;
    }

    expression
}

fn parse_grouped_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    parser.next_token(); // 跳过左括号

    let exp = parser.parse_expression(LOWEST);

    if !parser.expect_peek(TokenType::RPAREN) {
        return None;
    }

    exp
}

fn parse_if_expression(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    let mut if_exp = Box::new(crate::ast::IfExpression::new(parser.cur_token.clone()));

    if !parser.expect_peek(TokenType::LPAREN) {
        return None;
    }

    parser.next_token();

    // 解析条件表达式
    let condition = parser.parse_expression(LOWEST);
    if condition.is_none() {
        return None;
    }

    if_exp.condition = condition.unwrap();

    if !parser.expect_peek(TokenType::RPAREN) {
        return None;
    }

    if !parser.expect_peek(TokenType::LBRACE) {
        return None;
    }

    // 解析 consequence
    let consequence = parse_block_statement(parser);
    if consequence.is_some() {
        if_exp.consequence = consequence.unwrap();
    }

    if parser.peek_token_is(TokenType::ELSE) {
        parser.next_token();
        if !parser.expect_peek(TokenType::LBRACE) {
            return None;
        }

        // 解析 alternative
        if_exp.alternative = parse_block_statement(parser);
    }

    Some(if_exp as Box<dyn Expression>)
}

fn parse_block_statement(parser: &mut Parser) -> Option<BlockStatement> {
    let mut block = BlockStatement::new(parser.cur_token.clone());
    parser.next_token();

    while !parser.cur_token_is(TokenType::RBRACE) && !parser.cur_token_is(TokenType::EOF) {
        let stmt = parser.parse_statement();
        if let Some(statement) = stmt {
            block.statements.push(statement);
        }

        parser.next_token();
    }

    Some(block)
}

fn parse_function_literal(parser: &mut Parser) -> Option<Box<dyn Expression>> {
    let mut function_literal = Box::new(FunctionLiteral::new(parser.cur_token.clone()));
    if !parser.expect_peek(TokenType::LPAREN) {
        return None;
    }

    // 解析参数列表
    function_literal.parameters = parse_function_parameters(parser);

    // 解析函数体
    if !parser.expect_peek(TokenType::LBRACE) {
        return None;
    }

    function_literal.body = parse_block_statement(parser).unwrap_or_else(|| BlockStatement::new(Token::new_illegal()));

    Some(function_literal)
}

fn parse_function_parameters(parser: &mut Parser) -> Vec<Identifier> {
    let mut parameters = Vec::new();
    if parser.peek_token_is(TokenType::RPAREN) {
        parser.next_token(); // 跳过右括号
        return parameters;
    }

    parser.next_token();
    
    loop {
        let param = Identifier{
            token: parser.cur_token.clone(),
            value: parser.cur_token.literal.clone(),
        };
        
        parameters.push(param);
        
        if !parser.peek_token_is(TokenType::COMMA) {
            break; // 如果下一个 token 不是逗号，结束参数解析
        }
        
        parser.next_token(); // 跳过逗号
        parser.next_token(); // 跳过逗号
    }
    
    if !parser.expect_peek(TokenType::RPAREN) {
        return vec![]; // 如果没有正确的右括号，返回空参数列表
    }
    
    parameters
}

fn parse_call_expression(left: Box<dyn Expression>, parser: &mut Parser) -> Box<dyn Expression> {
    let mut call_expr = Box::new(crate::ast::CallExpression {
        token: parser.cur_token.clone(),
        function: left,
        arguments: Vec::new(),
    });

    call_expr.arguments = parse_call_arguments(parser);

    call_expr
}

fn parse_call_arguments(parser: &mut Parser) -> Vec<Box<dyn Expression>> {
    let mut args = Vec::new();
    if parser.peek_token_is(TokenType::RPAREN) {
        parser.next_token(); // 跳过右括号
        return args; // 如果没有参数，直接返回空列表
    }

    parser.next_token();

    loop {
        let expr = parser.parse_expression(LOWEST);
        if let Some(expression) = expr {
            args.push(expression);
        } else {
            parser.add_error("无法解析函数调用参数".to_string());
            return args; // 如果解析失败，返回已解析的参数列表
        }

        if !parser.peek_token_is(TokenType::COMMA) {
            break
        }
        parser.next_token();
        parser.next_token();
    }

    // 如果下一个 token 不是右括号，抛出错误
    if !parser.expect_peek(TokenType::RPAREN) {
        return args
    }

    args
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use crate::ast::Statement;
    use std::any::Any;

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
                Some(expr_stmt) => match &expr_stmt.expression {
                    Some(expr) => {
                        if let Some(ident) = expr.as_identifier() {
                            assert_eq!(ident.value, "foobar");
                            assert_eq!(ident.token_literal(), "foobar");
                        } else {
                            panic!("expression is not Identifier");
                        }
                    }
                    None => panic!("expression is None"),
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
            Some(stmt) => match stmt.as_expression_statement() {
                None => panic!("statement is not ExpressionStatement"),
                Some(expr_stmt) => match &expr_stmt.expression {
                    None => panic!("expression is None"),
                    Some(expr) => {
                        if let Some(ident) = expr.as_integer_literal() {
                            assert_eq!(ident.value, 5);
                            assert_eq!(ident.token_literal(), "5");
                        } else {
                            panic!("expression is not IdentifierLiteral");
                        }
                    }
                },
            },
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestCase {
            input: &'static str,
            operator: &'static str,
            value: Box<dyn Any>,
        }

        let tests = vec![
            TestCase {
                input: "!5;",
                operator: "!",
                value: Box::new(5),
            },
            TestCase {
                input: "-15;",
                operator: "-",
                value: Box::new(15),
            },
            TestCase {
                input: "!true;",
                operator: "!",
                value: Box::new(true),
            },
            TestCase {
                input: "!false;",
                operator: "!",
                value: Box::new(false),
            },
        ];

        for test in tests {
            let mut l = Lexer::new(test.input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);

            match program.statements.get(0) {
                None => panic!("statement index 0 is None"),
                Some(stmt) => match stmt.as_expression_statement() {
                    None => panic!("statement is not ExpressionStatement"),
                    Some(expr_stmt) => match &expr_stmt.expression {
                        None => panic!("expression is None"),
                        Some(expr) => {
                            if let Some(prefix_expr) = expr.as_prefix_expression() {
                                assert_eq!(prefix_expr.operator, test.operator);
                                match &prefix_expr.right {
                                    None => panic!("right expression is None"),
                                    Some(right) => {
                                        test_literal_expression(&right, &*test.value)
                                    }
                                }
                            } else {
                                panic!("expression is not PrefixExpression");
                            }
                        }
                    },
                },
            }
        }
    }

    fn test_integer_literal(right: &Box<dyn Expression>, value: i64) {
        match right.as_integer_literal() {
            Some(int_lit) => {
                assert_eq!(int_lit.value, value);
                assert_eq!(int_lit.token_literal(), value.to_string());
            }
            None => panic!("right expression is not IntegerLiteral"),
        }
    }

    fn test_identifier(exp: &Box<dyn Expression>, value: &str) {
        match exp.as_identifier() {
            Some(ident) => {
                assert_eq!(ident.value, value);
                assert_eq!(ident.token_literal(), value);
            }
            None => panic!("right expression is not Identifier"),
        }
    }

    fn test_literal_expression(exp: &Box<dyn Expression>, value: &dyn Any) {
        if let Some(int_val) = value.downcast_ref::<i64>() {
            test_integer_literal(exp, *int_val);
        } else if let Some(ident_val) = value.downcast_ref::<i32>() {
            test_integer_literal(exp, *ident_val as i64);
        } else if let Some(str_val) = value.downcast_ref::<&str>() {
            test_identifier(exp, str_val);
        } else if let Some(boolean_val) = value.downcast_ref::<bool>() {
            test_boolean_literal(exp, *boolean_val);
        } else {
            // 如果不是整数或标识符，抛出错误
            panic!("unsupported literal type");
        }
    }

    fn test_boolean_literal(exp: &Box<dyn Expression>, value: bool) {
        match exp.as_boolean() {
            Some(boolean) => {
                assert_eq!(boolean.value, value);
                assert_eq!(boolean.token_literal(), value.to_string());
            }
            None => panic!("expression is not Boolean"),
        }
    }

    fn test_infix_expression(
        exp: &dyn Expression,
        left_value: &dyn Any,
        operator: &str,
        right_value: &dyn Any,
    ) {
        match exp.as_infix_expression() {
            Some(infix_expr) => {
                test_literal_expression(&infix_expr.left, left_value);
                assert_eq!(infix_expr.operator, operator);
                test_literal_expression(&infix_expr.right, right_value);
            }
            None => panic!("expression is not InfixExpression"),
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct TestCase {
            input: &'static str,
            left_value: Box<dyn Any>,
            operator: &'static str,
            right_value: Box<dyn Any>,
        }

        let tests = vec![
            TestCase {
                input: "5 + 6;",
                left_value: Box::new(5),
                operator: "+",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 - 6;",
                left_value: Box::new(5),
                operator: "-",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 * 6;",
                left_value: Box::new(5),
                operator: "*",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 / 6;",
                left_value: Box::new(5),
                operator: "/",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 > 6;",
                left_value: Box::new(5),
                operator: ">",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 < 6;",
                left_value: Box::new(5),
                operator: "<",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 == 6;",
                left_value: Box::new(5),
                operator: "==",
                right_value: Box::new(6),
            },
            TestCase {
                input: "5 != 6;",
                left_value: Box::new(5),
                operator: "!=",
                right_value: Box::new(6),
            },
            TestCase{input:"true == true", left_value: Box::new(true), operator: "==", right_value: Box::new(true),},
            TestCase{input:"true != false", left_value: Box::new(true), operator: "!=", right_value: Box::new(false),},
            TestCase{input:"false == false", left_value: Box::new(false), operator: "==", right_value: Box::new(false),},
        ];

        for test in tests {
            let mut l = Lexer::new(test.input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);

            match program.statements.get(0) {
                None => panic!("statement index 0 is None"),
                Some(stmt) => match stmt.as_expression_statement() {
                    None => panic!("statement is not ExpressionStatement"),
                    Some(expr_stmt) => match &expr_stmt.expression {
                        None => panic!("expression is None"),
                        Some(expr) => {
                            if let Some(infix_expr) = expr.as_infix_expression() {
                                test_infix_expression(infix_expr, &*test.left_value, test.operator, &*test.right_value);
                            } else {
                                panic!("expression is not InfixExpression");
                            }
                        }
                    },
                },
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase {
            input: &'static str,
            expected: &'static str,
        }

        let tests = vec![
            TestCase {input: "-a * b", expected: "((-a) * b)"},
            TestCase {input: "!-a", expected: "(!(-a))"},
            TestCase {input: "a+b+c", expected: "((a + b) + c)"},
            TestCase {input: "a+b-c", expected: "((a + b) - c)"},
            TestCase {input: "a*b*c", expected: "((a * b) * c)"},
            TestCase {input: "a*b/c", expected: "((a * b) / c)"},
            TestCase {input: "a+b/c", expected: "(a + (b / c))"},
            TestCase {input: "a+b*c+d/e-f", expected: "(((a + (b * c)) + (d / e)) - f)"},
            TestCase {input: "3+4; -5 *5", expected: "(3 + 4)((-5) * 5)"},
            TestCase {input: "5 > 4 == 3 < 4", expected: "((5 > 4) == (3 < 4))"},
            TestCase {input: "5 < 4 != 3 > 4", expected: "((5 < 4) != (3 > 4))"},
            TestCase {input: "3+4*5 == 3 * 1 + 4 * 5", expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
            TestCase {
                input: "5 + 6 * 7 - 8 / 4;",
                expected: "((5 + (6 * 7)) - (8 / 4))",
            },
            TestCase {
                input: "3 + 4 * 5 - 6 / 2;",
                expected: "((3 + (4 * 5)) - (6 / 2))",
            },
            TestCase {
                input: "1 + 2 * 3 + 4;",
                expected: "((1 + (2 * 3)) + 4)",
            },
            TestCase{input: "true", expected: "true"},
            TestCase{input: "false", expected: "false"},
            TestCase{input: "3 > 5 == false", expected: "((3 > 5) == false)"},
            TestCase{input: "3 < 5 == false", expected: "((3 < 5) == false)"},
            TestCase{input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)"},
            TestCase{input: "(5 + 5) * 2", expected: "((5 + 5) * 2)"},
            TestCase{input: "2 / (5 + 5)", expected: "(2 / (5 + 5))"},
            TestCase{input: "2 * (5 + 5)", expected: "(2 * (5 + 5))"},
            TestCase{input: "-(5 + 5)", expected: "(-(5 + 5))"},
            TestCase{input: "!(true == true)", expected: "(!(true == true))"},
        ];

        for test in tests {
            let mut l = Lexer::new(test.input);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.to_string();
            assert_eq!(actual, test.expected, "input: {}", test.input);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = r#"
        true;
        false;
        "#;

        let tests = vec![
            true,
            false,
        ];

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 2);

        for (i, stmt) in program.statements.iter().enumerate() {
            match stmt.as_expression_statement() {
                Some(expr_stmt) => match &expr_stmt.expression {
                    Some(expr) => {
                        if let Some(boolean) = expr.as_boolean() {
                            assert_eq!(boolean.value, tests[i]);
                            assert_eq!(boolean.token_literal(), boolean.value.to_string());
                        } else {
                            panic!("expression is not Boolean");
                        }
                    }
                    None => panic!("expression is None"),
                },
                None => panic!("statement is not ExpressionStatement"),
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r#"
        if (x < y) {
            x;
        }
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0) {
            None => panic!("statement index 0 is None"),
            Some(stmt) => match stmt.as_expression_statement() {
                None => panic!("statement is not ExpressionStatement"),
                Some(expr_stmt) => match &expr_stmt.expression {
                    None => panic!("expression is None"),
                    Some(expr) => {
                        match expr.as_if_expression() {
                            None => panic!("expression is not IfExpression"),
                            Some(if_expr) => {
                                test_infix_expression(&*if_expr.condition, &"x", "<", &"y");
                                // test consequence
                                assert_eq!(if_expr.consequence.statements.len(), 1);
                                match if_expr.consequence.statements.get(0).unwrap().as_expression_statement() {
                                    None => panic!("statement is not ExpressionStatement"),
                                    Some(consequent_stmt) => {
                                        match &consequent_stmt.expression {
                                            None => panic!("expression is None"),
                                            Some(consequent_expr) => {
                                                test_identifier(consequent_expr, "x");
                                            }
                                        }
                                    }
                                }

                                // test alternative
                                assert!(if_expr.alternative.is_none());
                            }
                        }
                    }
                },
            },
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = r#"
        if (x < y) { x } else { y }
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0).unwrap().as_expression_statement() {
            None => panic!("statement is not ExpressionStatement"),

            Some(expr_stmt) => match &expr_stmt.expression {
                None => panic!("expression is None"),
                Some(expr) => {
                    match expr.as_if_expression() {
                        None => panic!("expression is not IfExpression"),
                        Some(if_expr) => {
                            test_infix_expression(&*if_expr.condition, &"x", "<", &"y");

                            // test consequence
                            assert_eq!(if_expr.consequence.statements.len(), 1);
                            match if_expr.consequence.statements.get(0).unwrap().as_expression_statement() {
                                None => panic!("statement is not ExpressionStatement"),
                                Some(consequent_stmt) => {
                                    match &consequent_stmt.expression {
                                        None => panic!("expression is None"),
                                        Some(consequent_expr) => {
                                            test_identifier(consequent_expr, "x");
                                        }
                                    }
                                }
                            }

                            // test alternative
                            assert!(if_expr.alternative.is_some());
                            let alt = if_expr.alternative.as_ref().unwrap();
                            assert_eq!(alt.statements.len(), 1);
                            match alt.statements.get(0).unwrap().as_expression_statement() {
                                None => panic!("statement is not ExpressionStatement"),
                                Some(alternative_stmt) => {
                                    match &alternative_stmt.expression {
                                        None => panic!("expression is None"),
                                        Some(alternative_expr) => {
                                            test_identifier(alternative_expr, "y");
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = r#"
        fn(x, y) {
            x + y;
        }
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);
        
        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0).unwrap().as_expression_statement().
            unwrap().expression.as_ref().unwrap().as_function_literal() {
            None => panic!("expression is not FunctionLiteral"),
            Some(function_literal) => {
                assert_eq!(function_literal.parameters.len(), 2);

                // parameters
                let param0: Box<dyn Expression> = Box::new(function_literal.parameters[0].clone());
                test_literal_expression(&param0, &"x");
                let param1: Box<dyn Expression> = Box::new(function_literal.parameters[1].clone());
                test_literal_expression(&param1, &"y");

                // body
                assert_eq!(function_literal.body.statements.len(), 1);

                match function_literal.body.statements.get(0).unwrap().as_expression_statement() {
                    None => panic!("statement is not ExpressionStatement"),
                    Some(expr_stmt) => {
                        let expression = expr_stmt.expression.as_ref().unwrap();
                        test_infix_expression(&**expression, &"x", "+", &"y");
                    }
                }
            },
        }
    }

    #[test]
    fn test_function_parameters_parsing() {
        let tests: Vec<(&str, Vec<&str>)> = vec![
            ("fn(){};", vec![]),
            ("fn(x){};", vec!["x"]),
            ("fn(x, y, z){};", vec!["x", "y", "z"]),
        ];

        for test in tests {
            let mut l = Lexer::new(test.0);
            let mut p = Parser::new(&mut l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);

            let function_literal = program.statements.get(0).unwrap()
                .as_expression_statement().unwrap()
                .expression.as_ref().unwrap().as_function_literal().unwrap();

            assert_eq!(function_literal.parameters.len(), test.1.len());

            for (i, param) in function_literal.parameters.iter().enumerate() {
                let p: Box<dyn Expression> = Box::new(param.clone());
                test_literal_expression(&p, test.1.get(i).unwrap());
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = r#"
        add(1, 2 * 3, 4 + 5);
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0).unwrap().as_expression_statement().unwrap().
            expression.as_ref().unwrap()
            .as_call_expression()
        {
            None => panic!("expression is not CallExpression"),
            Some(call_expr) => {
                test_identifier(&call_expr.function, "add");
                assert_eq!(call_expr.arguments.len(), 3);

                // 测试第一个参数
                test_literal_expression(&call_expr.arguments[0], &1i64);
                // 测试第二个参数
                test_infix_expression(&*call_expr.arguments[1], &2i64, "*", &3i64);
                // 测试第三个参数
                test_infix_expression(&*call_expr.arguments[2], &4i64, "+", &5i64);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing_empty() {
        let input = r#"
        add();
        "#;

        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match program.statements.get(0).unwrap().as_expression_statement().unwrap().
            expression.as_ref().unwrap()
            .as_call_expression()
        {
            None => panic!("expression is not CallExpression"),
            Some(call_expr) => {
                test_identifier(&call_expr.function, "add");
                assert_eq!(call_expr.arguments.len(), 0);
            }
        }
    }
}

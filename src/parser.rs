use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::TokenType::{ASSIGN, IDENT, LET, SEMICOLON};
use crate::token::{Token, TokenType};
use std::panic::catch_unwind;

pub struct Parser<'a> {
    l: &'a mut Lexer,

    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut Lexer) -> Self {
        let mut p = Parser {
            l,
            cur_token: Token {
                tp: TokenType::ILLEGAL,
                literal: "".to_string(),
            },
            peek_token: Token {
                tp: TokenType::ILLEGAL,
                literal: "".to_string(),
            },
        };
        // 读取两个词法单元，以设置 cur_token 和 peek_token
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Program {
        // 创建一个新的 Program 实例
        let mut program = Program { statements: vec![] };

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
            _ => None, // 其他语句类型可以在这里添加
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = Box::new(LetStatement {
            token: self.cur_token.clone(),
            name: Box::new(Identifier {
                token: Token {
                    tp: TokenType::ILLEGAL,
                    literal: "".to_string(),
                },
                value: "".to_string(), // 初始值为空，稍后会被更新
            }),
            value: Box::new(Identifier {
                token: Token{
                    tp: TokenType::ILLEGAL,
                    literal: "".to_string(),
                },
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
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

        assert_ne!(program.statements.len(), 0);
        assert_eq!(program.statements.len(), 3);

        let tests = vec![("x", 5), ("y", 10), ("foobar", 838383)];

        for (i, tt) in tests.iter().enumerate() {
            if let Some(stmt) = program.statements.get(i) {
                // TODO:
                println!(
                    "### i:{:?} {:?} {:?} {:?}",
                    i,
                    stmt.statement_node(),
                    stmt.token_literal(),
                    tt
                );

                test_let_statement(stmt, tt)
            } else {
                assert!(false, "no statement at index {}", i);
            }
        }
    }

    fn test_let_statement(stmt: &Box<dyn Statement>, tt: &(&str, i32)) {
        assert_eq!(stmt.token_literal(), "let");
        if let Some(let_stmt) = stmt.as_let_statement() {
            assert_eq!(let_stmt.name.value, tt.0);
        } else {
            panic!("stmt is not LetStatement");
        }
    }
}

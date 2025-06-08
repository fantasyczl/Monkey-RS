use crate::token::Token;
use crate::token::TokenType;

pub struct Lexer {
    input: String,
    position: usize,      // 所输入字符串中的当前位置（指向当前字符）
    read_position: usize, // 所输入字符串中的当前读取位置（指向当前字符之后的一个字符)
    ch: char,             // 当前正在查看的字符
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0'; // EOF
        } else {
            self.ch = self.input[self.read_position..].chars().next().unwrap();
        }
        self.position = self.read_position;
        self.read_position += self.ch.len_utf8();
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token;

        // 跳过空格和换行符
        self.skip_whitespace();

        match self.ch {
            '=' => {
                token = Token::new(TokenType::ASSIGN, "=");
            }
            ';' => {
                token = Token::new(TokenType::SEMICOLON, ";");
            }
            '!' => {
                token = Token::new(TokenType::BANG, "!");
            }
            '-' => {
                token = Token::new(TokenType::MINUS, "-");
            }
            '/' => {
                token = Token::new(TokenType::SLASH, "/");
            }
            '*' => {
                token = Token::new(TokenType::ASTERISK, "*");
            }
            '(' => {
                token = Token::new(TokenType::LPAREN, "(");
            }
            ')' => {
                token = Token::new(TokenType::RPAREN, ")");
            }
            '{' => {
                token = Token::new(TokenType::LBRACE, "{");
            }
            '}' => {
                token = Token::new(TokenType::RBRACE, "}");
            }
            ',' => {
                token = Token::new(TokenType::COMMA, ",");
            }
            '+' => {
                token = Token::new(TokenType::PLUS, "+");
            }
            '<' => {
                token = Token::new(TokenType::LT, "<");
            }
            '>' => {
                token = Token::new(TokenType::GT, ">");
            }
            '\0' => {
                token = Token::new(TokenType::EOF, "");
            }
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let tp = TokenType::lookup_ident(&literal);
                    return Token::new(tp, &literal);
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token::new(TokenType::INT, &literal);
                } else {
                    token = Token::new(TokenType::ILLEGAL, self.ch.to_string().as_str());
                }
            }
        }

        self.read_char();

        token
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

#[cfg(test)]
mod tests {
    use super::*; // 导入父模块所有的内容
    use crate::token::TokenType;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = vec![
            (TokenType::ASSIGN, "="),
            (TokenType::PLUS, "+"),
            (TokenType::LPAREN, "("),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RBRACE, "}"),
            (TokenType::COMMA, ","),
            (TokenType::SEMICOLON, ";"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (i, tt) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(
                token.tp,
                tt.0,
                "tests[{}] - token type wrong. expected={}, got={}",
                i,
                tt.0.as_str(),
                token.tp.as_str()
            );
            assert_eq!(
                token.literal, tt.1,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, tt.1, token.literal
            );
        }
    }

    #[test]
    fn test_next_token2() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
            x + y;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        "#;

        let tests = vec![
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "5"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
        ];

        let mut lexer = Lexer::new(input);
        for (i, tt) in tests.iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(
                token.tp,
                tt.0,
                "tests[{}] - token type wrong. expected={}, got={}",
                i,
                tt.0.as_str(),
                token.tp.as_str()
            );
            assert_eq!(
                token.literal, tt.1,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, tt.1, token.literal
            );
        }
    }
}

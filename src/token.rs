pub struct Token {
    pub tp: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(tp: TokenType, literal: &str) -> Token {
        Token {
            tp,
            literal: literal.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    //  标识符+字面量
    IDENT, // add, foobar, x, y, ...
    INT,   // 123456

    // 运算符
    ASSIGN,   // =
    PLUS,     // +
    MINUS,    // -
    BANG,     // !
    ASTERISK, // *
    SLASH,    // /

    // 关系运算符
    LT, // <
    GT, // >
    EQ, // ==

    // 分隔符
    COMMA,     // ,
    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    // 关键字
    FUNCTION, // function
    LET,      // let
    IF,       // if
    ELSE,     // else
    RETURN,   // return
    TRUE,     // true
    FALSE,    // false
}

impl TokenType {
    pub fn as_str(&self) -> &'static str {
        match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::BANG => "!",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",
            TokenType::LT => "<",
            TokenType::GT => ">",
            TokenType::EQ => "==",
            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",
            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::RETURN => "RETURN",
            TokenType::TRUE => "TRUE",
            TokenType::FALSE => "FALSE",
        }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            _ => TokenType::IDENT,
        }
    }
}

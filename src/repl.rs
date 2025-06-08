use std::io::BufRead;
use crate::lexer::Lexer;
use crate::token::TokenType;

const PROMPT: &str = ">> ";

pub fn start_repl(input : &mut dyn std::io::Read, out: &mut dyn std::io::Write) {
    let mut scanner = std::io::BufReader::new(input);

    loop {
        out.write_fmt(format_args!("{}", PROMPT)).unwrap();
        out.flush().unwrap();

        let mut line = String::new();
        if scanner.read_line(&mut line).unwrap() == 0 {
            out.write_all(b"Exiting REPL...\n").unwrap();
            break; // EOF
        }

        let line = line.trim_end(); // 去除末尾的换行符
        if line.is_empty() {
            continue; // 如果输入为空，继续下一次循环
        }

        let mut l = Lexer::new(line);

        loop {
            let token = l.next_token();
            if token.tp == TokenType::EOF {
                break; // 读取到 EOF，退出循环
            }
            writeln!(out, "{:?}", token).unwrap();
        }
    }
}
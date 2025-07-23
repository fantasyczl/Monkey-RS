use std::io::BufRead;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::{evaluator, object};

const PROMPT: &str = ">> ";

pub fn start_repl(input : &mut dyn std::io::Read, out: &mut dyn std::io::Write) {
    let mut scanner = std::io::BufReader::new(input);
    let env = &mut object::Environment::new();

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
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        if !p.errors().is_empty() {
            // 输出错误信息
            print_error(out, p.errors());
            continue; // 如果解析出错，继续下一次循环
        }

        if let Some(evaluated) = evaluator::eval(&program, env) {
            if evaluated.type_name() == "Null" {
                writeln!(out, "null").unwrap();
            } else {
                writeln!(out, "{}", evaluated.inspect()).unwrap();
            }
        }
    }
}

const MONKEY_FACE: &str = r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

fn print_error(out: &mut dyn std::io::Write, errors: &Vec<String>) {
    if errors.is_empty() {
        return; // 如果没有错误，直接返回
    }

    writeln!(out, "{}", MONKEY_FACE).unwrap();
    writeln!(out, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(out, " parser errors:").unwrap();

    for msg in errors {
        writeln!(out, "\t{}", msg).unwrap();
    }
    out.flush().unwrap();
}
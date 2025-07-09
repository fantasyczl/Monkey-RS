use crate::ast::{Boolean, Expression, ExpressionStatement, IntegerLiteral, Node, Statement};
use crate::object::Object;

pub fn eval(node: &dyn Node) ->  Option<Box<dyn Object>> {
    if let Some(program) = node.as_program() {
        return eval_statements(&program.statements);
    } else if let Some(integer_literal) = node.as_any().downcast_ref::<IntegerLiteral>() {
        return Some(Box::new(crate::object::Integer { value: integer_literal.value }));
    } else if let Some(boolean) = node.as_any().downcast_ref::<Boolean>() {
        return Some(Box::new(crate::object::Boolean { value: boolean.value }));
    } else if let Some(expr) = node.as_any().downcast_ref::<ExpressionStatement>() {
        if let Some(expr) = expr.expression.as_ref() {
            return eval(expr.as_ref());
        }
    }

    None
}

fn eval_statements(statements: &[Box<dyn Statement>]) -> Option<Box<dyn Object>> {
    let mut object: Option<Box<dyn Object>> = None;

    for statement in statements {
        // let node: dyn Node = statement as dyn Node;
        object = eval(statement.as_ref());
    }

    object
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::object::Object;

    #[test]
    fn test_eval_integer_expression() {
        struct Case {
            input: &'static str,
            expected: i64,
        }

        let tests = vec![
            Case { input: "5", expected: 5 },
            Case { input: "10", expected: 10 },
            // TODO
            // Case { input: "-3", expected: -3 },
            // Case { input: "100 + 200", expected: 300 },
            // Case { input: "50 - 20", expected: 30 },
            // Case { input: "2 * 3", expected: 6 },
            // Case { input: "8 / 2", expected: 4 },
        ];

        for test in tests {
            let object = test_eval(test.input);
            test_integer_object(object, test.expected);
        }
    }

    fn test_eval(input: &'static str) -> Option<Box<dyn Object>> {
        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();

        eval(&program)
    }

    fn test_integer_object(object: Option<Box<dyn Object>>, expected: i64) {
        assert!(object.is_some(), "Expected an object, but got None");
        let object = object.unwrap();

        assert_eq!(object.type_name(), "Integer");
        let integer = object.as_integer().unwrap();
        assert_eq!(integer.value, expected);
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Case {
            input: &'static str,
            expected: bool,
        }

        let tests = vec![
            Case { input: "true", expected: true },
            Case { input: "false", expected: false },
        ];

        for test in tests {
            let object = test_eval(test.input);
            test_boolean_object(object, test.expected);
        }
    }

    fn test_boolean_object(object: Option<Box<dyn Object>>, expected: bool) {
        assert!(object.is_some(), "Expected an object, but got None");
        let object = object.unwrap();

        assert_eq!(object.type_name(), "Boolean");
        let boolean = object.as_boolean().unwrap();
        assert_eq!(boolean.value, expected);
    }
}
use crate::ast::{
    Boolean, ExpressionStatement, InfixExpression, IntegerLiteral, Node, PrefixExpression,
    Statement,
};
use crate::object;
use crate::object::{Object};

const NULL_OBJ: object::Null = object::Null;
const TRUE: object::Boolean = object::Boolean { value: true };
const FALSE: object::Boolean = object::Boolean { value: false };

macro_rules! new_error {
    ($fmt:expr, $($arg:tt)*) => {
        Box::new(object::Error {
            message: format!($fmt, $($arg)*),
        })
    };
}

pub fn eval(node: &dyn Node, env: &mut object::Environment) -> Option<Box<dyn Object>> {
    if let Some(program) = node.as_program() {
        return eval_program(&program.statements);
    } else if let Some(integer_literal) = node.as_any().downcast_ref::<IntegerLiteral>() {
        return Some(Box::new(crate::object::Integer {
            value: integer_literal.value,
        }));
    } else if let Some(boolean) = node.as_any().downcast_ref::<Boolean>() {
        return Some(native_bool_to_boolean_object(boolean.value));
    } else if let Some(expr) = node.as_any().downcast_ref::<ExpressionStatement>() {
        if let Some(expr) = expr.expression.as_ref() {
            return eval(expr.as_ref(), env);
        }
    } else if let Some(pre_expr) = node.as_any().downcast_ref::<PrefixExpression>() {
        if let Some(right_node) = pre_expr.right.as_ref() {
            let right = eval(right_node.as_ref(), env);
            if is_error(&right) {
                return right;
            }

            return eval_prefix_expression(&pre_expr.operator, right);
        }
    } else if let Some(infix_expr) = node.as_any().downcast_ref::<InfixExpression>() {
        let left = eval(infix_expr.left.as_ref(), env);
        if is_error(&left) {
            return left;
        }

        let right = eval(infix_expr.right.as_ref(), env);
        if is_error(&right) {
            return right;
        }

        return eval_infix_expression(&infix_expr.operator, left, right);
    } else if let Some(if_expr) = node.as_any().downcast_ref::<crate::ast::IfExpression>() {
        return eval_if_expression(if_expr);
    } else if let Some(block_stmt) = node.as_any().downcast_ref::<crate::ast::BlockStatement>() {
        return eval_block_statements(&block_stmt.statements);
    } else if let Some(return_stmt) = node.as_any().downcast_ref::<crate::ast::ReturnStatement>() {
        if let Some(expr) = return_stmt.return_value.as_ref() {
            let value = eval(expr.as_ref(), env);
            if is_error(&value) {
                return value;
            }

            if let Some(value_obj) = value {
                return Some(Box::new(object::ReturnValue { value: value_obj }));
            }
        }
    } else if let Some(let_stmt) = node.as_any().downcast_ref::<crate::ast::LetStatement>() {
        let val_node = let_stmt.value.as_ref();
        let val = eval(val_node, env);
        if is_error(&val) {
            return val;
        }

        if let Some(value) = val {
            env.set(let_stmt.name.value.clone(), value);
        }
    } else if let Some(identifier) = node.as_any().downcast_ref::<crate::ast::Identifier>() {
        return eval_identifier(identifier, env);
    }

    Some(Box::new(NULL_OBJ))
}

fn is_error(obj: &Option<Box<dyn Object>>) -> bool {
    match obj {
        None => false,
        Some(obj) => {
            obj.as_error().is_some()
        }
    }
}

fn eval_program(statements: &[Box<dyn Statement>]) -> Option<Box<dyn Object>> {
    let mut object: Option<Box<dyn Object>> = None;

    let env = &mut object::Environment::new();

    for statement in statements {
        object = eval(statement.as_ref(), env);

        if let Some(object_t) = object.as_ref() {
            if let Some(return_val) = object_t.as_return_value() {
                return Some(return_val.value.clone());
            } else if object_t.as_error().is_some() {
                return object;
            }
        }
    }

    object
}

fn eval_block_statements(statements: &[Box<dyn Statement>]) -> Option<Box<dyn Object>> {
    let mut result: Option<Box<dyn Object>> = None;

    let env = &mut object::Environment::new();

    for statement in statements {
        result = eval(statement.as_ref(), env);
        if let Some(object) = result.as_ref() {
            if object.type_name() == object::RETURN_VALUE_OBJ {
                return result;
            } else if object.type_name() == object::ERROR_OBJ {
                return result; // If an error is encountered, return it immediately
            }
        }
    }

    result
}

fn native_bool_to_boolean_object(input: bool) -> Box<dyn Object> {
    if input {
        Box::new(TRUE)
    } else {
        Box::new(FALSE)
    }
}

fn eval_prefix_expression(
    operator: &str,
    right: Option<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_operator_expression(right),
        _ => Some(new_error!(
            "unknown operator: {}{}",
            operator,
            right.unwrap().type_name()
        )),
    }
}

fn eval_bang_operator_expression(right: Option<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    match right {
        None => return Some(native_bool_to_boolean_object(true)), // !null is true
        Some(obj) => {
            if let Some(b) = obj.as_boolean() {
                return Some(native_bool_to_boolean_object(!b.value));
            } else if let Some(i) = obj.as_integer() {
                return Some(native_bool_to_boolean_object(i.value == 0));
            } else {
                None
            }
        }
    }
}

fn eval_minus_operator_expression(right: Option<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if let Some(obj) = right {
        if let Some(i) = obj.as_integer() {
            return Some(Box::new(object::Integer { value: -i.value }));
        } else {
            return Some(new_error!(
                "unknown operator: -{}",
                obj.type_name()
            ));
        }
    }
    None
}

fn eval_infix_expression(
    operator: &str,
    left: Option<Box<dyn Object>>,
    right: Option<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    match operator {
        "+" => eval_integer_infix_expression(left, right, |a, b| a + b, operator),
        "-" => eval_integer_infix_expression(left, right, |a, b| a - b, operator),
        "*" => eval_integer_infix_expression(left, right, |a, b| a * b, operator),
        "/" => eval_integer_infix_expression(left, right, |a, b| a / b, operator),
        "==" => eval_boolean_infix_expression(left, right, |a, b| a == b, Some(|a, b| a == b), operator),
        "!=" => eval_boolean_infix_expression(left, right, |a, b| a != b, Some(|a, b| a != b), operator),
        "<" => eval_boolean_infix_expression(left, right, |a, b| a < b, None, operator),
        ">" => eval_boolean_infix_expression(left, right, |a, b| a > b, None, operator),
        "<=" => eval_boolean_infix_expression(left, right, |a, b| a <= b, None, operator),
        ">=" => eval_boolean_infix_expression(left, right, |a, b| a >= b, None, operator),
        _ => check_type_mismatch(operator, left, right),
    }
}

fn check_type_mismatch(
    operator: &str,
    left: Option<Box<dyn Object>>,
    right: Option<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    if let (Some(left_obj), Some(right_obj)) = (&left, &right) {
        if left_obj.type_name() != right_obj.type_name() {
            return Some(new_error!(
                "Type mismatch: {} {} {}",
                left_obj.type_name(),
                operator,
                right_obj.type_name()
            ));
        }
    }
    Some(new_error!(
        "unknown operator: {} {} {}",
        left.as_ref().map_or("None".to_string(), |l| l.type_name()),
        operator,
        right.as_ref().map_or("None".to_string(), |r| r.type_name()),
    ))
}

fn eval_integer_infix_expression(
    left: Option<Box<dyn Object>>,
    right: Option<Box<dyn Object>>,
    op: fn(i64, i64) -> i64,
    operator: &str,
) -> Option<Box<dyn Object>> {
    if let (Some(left_obj), Some(right_obj)) = (left, right) {
        if let (Some(left_int), Some(right_int)) = (left_obj.as_integer(), right_obj.as_integer()) {
            return Some(Box::new(object::Integer {
                value: op(left_int.value, right_int.value),
            }));
        } else {
            return Some(new_error!(
                "Type mismatch: {} {} {}",
                left_obj.type_name(),
                operator,
                right_obj.type_name()
            ));
        }
    }
    None
}

fn eval_boolean_infix_expression(
    left: Option<Box<dyn Object>>,
    right: Option<Box<dyn Object>>,
    op: fn(i64, i64) -> bool,
    bool_op: Option<fn(bool, bool) -> bool>,
    operator: &str,
) -> Option<Box<dyn Object>> {
    if let (Some(left_obj), Some(right_obj)) = (left, right) {
        if let (Some(left_int), Some(right_int)) = (left_obj.as_integer(), right_obj.as_integer()) {
            return Some(native_bool_to_boolean_object(op(
                left_int.value,
                right_int.value,
            )));
        } else if let (Some(left_bool), Some(right_bool)) =
            (left_obj.as_boolean(), right_obj.as_boolean())
        {
            if let Some(bool_op) = bool_op {
                return Some(native_bool_to_boolean_object(bool_op(
                    left_bool.value,
                    right_bool.value,
                )));
            }
        } else {
            return Some(new_error!(
                "Type mismatch: {} {} {}",
                left_obj.type_name(),
                operator,
                right_obj.type_name()
            ));
        }
    }
    None
}

fn eval_if_expression(ie: &crate::ast::IfExpression) -> Option<Box<dyn Object>> {
    let env = &mut object::Environment::new();
    let condition = eval(ie.condition.as_ref(), env);
    if is_error(&condition) {
        return condition;
    }

    if let Some(cond_obj) = condition {
        if if_truthy(&*cond_obj) {
            return eval(&ie.consequence, env);
        } else if let Some(alternative) = &ie.alternative {
            return eval(alternative, env);
        } else {
            return Some(Box::new(NULL_OBJ));
        }
    }

    None
}

fn if_truthy(obj: &dyn Object) -> bool {
    if let Some(boolean) = obj.as_boolean() {
        boolean.value
    } else if let Some(integer) = obj.as_integer() {
        integer.value != 0
    } else if obj.type_name() == NULL_OBJ.type_name() {
        false
    } else {
        true
    }
}

fn eval_identifier(
    identifier: &crate::ast::Identifier,
    env: &mut object::Environment,
) -> Option<Box<dyn Object>> {
    if let Some(value) = env.get(&identifier.value) {
        Some(value.clone_box())
    } else {
        Some(new_error!("identifier not found: {}", identifier.value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        struct Case {
            input: &'static str,
            expected: i64,
        }

        let tests = vec![
            Case {
                input: "5",
                expected: 5,
            },
            Case {
                input: "10",
                expected: 10,
            },
            Case {
                input: "-3",
                expected: -3,
            },
            Case {
                input: "100 + 200",
                expected: 300,
            },
            Case {
                input: "50 - 20",
                expected: 30,
            },
            Case {
                input: "2 * 3",
                expected: 6,
            },
            Case {
                input: "8 / 2",
                expected: 4,
            },
            Case {
                input: "2 + 3 * 4",
                expected: 14,
            }, // Test operator precedence
            Case {
                input: "(1 + 2) * 3",
                expected: 9,
            }, // Test parentheses
            Case {
                input: "10 - (2 + 3)",
                expected: 5,
            }, // Test parentheses with subtraction
            Case {
                input: "2 * (3 + 4)",
                expected: 14,
            }, // Test parentheses with multiplication
            Case {
                input: "10 / (2 - 1)",
                expected: 10,
            }, // Test parentheses with division
            Case {
                input: "5 + 3 - 2",
                expected: 6,
            }, // Test mixed operations
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
        let env = &mut object::Environment::new();

        eval(&program, env)
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
            Case {
                input: "true",
                expected: true,
            },
            Case {
                input: "false",
                expected: false,
            },
            Case {
                input: "5 == 5",
                expected: true,
            },
            Case {
                input: "5 != 10",
                expected: true,
            },
            Case {
                input: "5 < 10",
                expected: true,
            },
            Case {
                input: "10 > 5",
                expected: true,
            },
            Case {
                input: "true == true",
                expected: true,
            },
            Case {
                input: "false == false",
                expected: true,
            },
            Case {
                input: "true != false",
                expected: true,
            },
            Case {
                input: "false != true",
                expected: true,
            },
            Case {
                input: "(1 < 2) == true",
                expected: true,
            },
            Case {
                input: "(1 < 2) == false",
                expected: false,
            },
            Case {
                input: "(1 > 2) == false",
                expected: true,
            },
            Case {
                input: "(1 > 2) == true",
                expected: false,
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
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

    #[test]
    fn test_eval_bang_operator() {
        struct Case {
            input: &'static str,
            expected: bool,
        }

        let tests = vec![
            Case {
                input: "!true",
                expected: false,
            },
            Case {
                input: "!false",
                expected: true,
            },
            Case {
                input: "!5",
                expected: false,
            },
            Case {
                input: "!!true",
                expected: true,
            },
            Case {
                input: "!!false",
                expected: false,
            },
            Case {
                input: "!!5",
                expected: true,
            },
        ];

        for test in tests {
            print!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            test_boolean_object(object, test.expected);
        }
    }

    #[test]
    fn test_eval_if_expression() {
        struct Case {
            input: &'static str,
            expected: Option<i64>,
        }

        let tests = vec![
            Case {
                input: "if (true) { 10 }",
                expected: Some(10),
            },
            Case {
                input: "if (false) { 10 }",
                expected: None,
            },
            Case {
                input: "if (1 < 2) { 20 } else { 30 }",
                expected: Some(20),
            },
            Case {
                input: "if (1 > 2) { 20 } else { 30 }",
                expected: Some(30),
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            if let Some(obj) = object {
                if let Some(integer) = obj.as_integer() {
                    assert_eq!(integer.value, test.expected.unwrap());
                } else {
                    test_null_object(&obj);
                }
            } else {
                assert!(test.expected.is_none(), "Expected an object, but got None");
            }
        }
    }

    fn test_null_object(object: &Box<dyn Object>) {
        assert_eq!(object.type_name(), "Null");
    }

    #[test]
    fn test_eval_return_statement() {
        struct Case {
            input: &'static str,
            expected: Option<i64>,
        }

        let tests = vec![
            // Case {
            //     input: "return 10;",
            //     expected: Some(10),
            // },
            // Case {
            //     input: "return 10;9;",
            //     expected: Some(10),
            // },
            // Case {
            //     input: "return 20; return 30;",
            //     expected: Some(20),
            // },
            // Case {
            //     input: "if (true) { return 40; }",
            //     expected: Some(40),
            // },
            // Case {
            //     input: "if (false) { return 50; } else { return 60; }",
            //     expected: Some(60),
            // },
            // Case {
            //     input: "9; return 2 * 5; 10;",
            //     expected: Some(10),
            // },
            // TODO: Fix the return statement tests
            Case {
                input: "let x = 5; return x;",
                expected: Some(5),
            },
            // Case {
            //     input: "if (10 > 1) { if (true) { return 15; } return 1; } else { return 20; }",
            //     expected: Some(15),
            // },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            test_integer_object(object, test.expected.unwrap());
        }
    }

    #[test]
    fn test_error_handling() {
        struct Case {
            input: &'static str,
            expected_error: &'static str,
        }

        let tests = vec![
            Case {
                input: "5 + true;",
                expected_error: "Type mismatch: Integer + Boolean",
            },
            Case {
                input: "5 + true; 5;",
                expected_error: "Type mismatch: Integer + Boolean",
            },
            Case {
                input: "-true;",
                expected_error: "unknown operator: -Boolean",
            },
            Case {
                input: "true + false;",
                expected_error: "Type mismatch: Boolean + Boolean",
            },
            Case {
                input: "5; true + false;",
                expected_error: "Type mismatch: Boolean + Boolean",
            },
            Case {
                input: "if (10 > 1) { return true + false; }",
                expected_error: "Type mismatch: Boolean + Boolean",
            },
            Case {
                input: "if (10 > 1) { return true + false; } else { return 10; }",
                expected_error: "Type mismatch: Boolean + Boolean",
            },
            Case {
                input: "foobar",
                expected_error: "identifier not found: foobar",
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            match object {
                None => {
                    // If the eval function returns None, it means an error occurred.
                    // We will assert that the expected error matches the actual error.
                    panic!("Expected an object, but got None");
                }
                Some(obj) => {
                    // If we get an object back, it should be an error object.
                    match obj.as_error() {
                        Some(error) => {
                            // Check if the error message matches the expected error.
                            assert_eq!(error.message, test.expected_error);
                        }
                        None => {
                            panic!("Expected an error object, but got: {}", obj.inspect());
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct Case {
            input: &'static str,
            expected: i64,
        }

        let tests = vec![
            Case {
                input: "let x = 5; x;",
                expected: 5,
            },
            Case {
                input: "let y = 10; y + 5;",
                expected: 15,
            },
            Case {
                input: "let z = 20; z - 5;",
                expected: 15,
            },
            Case {
                input: "let a = 1; let b = 2; a + b;",
                expected: 3,
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            test_integer_object(object, test.expected);
        }
    }
}

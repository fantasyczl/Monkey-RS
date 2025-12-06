use crate::ast::{
    Boolean, ExpressionStatement, InfixExpression, IntegerLiteral, Node, PrefixExpression,
    Statement,
};
use crate::object;
use crate::object::Object;
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

// Builtin functions
fn len_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(new_error!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    let arg = &args[0];
    if let Some(string_obj) = arg.as_string() {
        Some(Box::new(object::Integer {
            value: string_obj.value.len() as i64,
        }))
    } else if let Some(array_obj) = arg.as_array() {
        Some(Box::new(object::Integer {
            value: array_obj.elements.len() as i64,
        }))
    } else {
        Some(new_error!(
            "argument to `len` not supported, got {}",
            arg.type_name()
        ))
    }
}

fn first_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(new_error!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    let arg = &args[0];
    if let Some(array_obj) = arg.as_array() {
        if !array_obj.elements.is_empty() {
            Some(array_obj.elements[0].clone_box())
        } else {
            Some(Box::new(NULL_OBJ))
        }
    } else {
        Some(new_error!(
            "argument to `first` must be ARRAY, got {}",
            arg.type_name()
        ))
    }
}

fn last_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(new_error!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    let arg = &args[0];
    if let Some(array_obj) = arg.as_array() {
        if !array_obj.elements.is_empty() {
            Some(array_obj.elements[array_obj.elements.len() - 1].clone_box())
        } else {
            Some(Box::new(NULL_OBJ))
        }
    } else {
        Some(new_error!(
            "argument to `last` must be ARRAY, got {}",
            arg.type_name()
        ))
    }
}

fn rest_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(new_error!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    let arg = &args[0];
    if let Some(array_obj) = arg.as_array() {
        if array_obj.elements.len() > 1 {
            let new_elements = array_obj.elements[1..].to_vec();
            Some(Box::new(object::Array {
                elements: new_elements,
            }))
        } else {
            Some(Box::new(NULL_OBJ))
        }
    } else {
        Some(new_error!(
            "argument to `rest` must be ARRAY, got {}",
            arg.type_name()
        ))
    }
}

fn push_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 2 {
        return Some(new_error!(
            "wrong number of arguments. got={}, want=2",
            args.len()
        ));
    }

    let array_arg = &args[0];
    let element_arg = &args[1];

    if let Some(array_obj) = array_arg.as_array() {
        let mut new_elements = array_obj.elements.clone();
        new_elements.push(element_arg.clone_box());
        Some(Box::new(object::Array {
            elements: new_elements,
        }))
    } else {
        Some(new_error!(
            "first argument to `push` must be ARRAY, got {}",
            array_arg.type_name()
        ))
    }
}

fn puts_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    for arg in args.iter() {
        println!("{}", arg.inspect());
    }
    Some(Box::new(NULL_OBJ))
}

// 全局内置函数映射，惰性初始化
static BUILTINS: Lazy<HashMap<&'static str, object::Builtin>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("len", object::Builtin { func: len_builtin });
    m.insert("first",object::Builtin {func: first_builtin });
    m.insert("last", object::Builtin { func: last_builtin });
    m.insert("rest", object::Builtin { func: rest_builtin });
    m.insert("push", object::Builtin { func: push_builtin });
    m.insert("puts", object::Builtin { func: puts_builtin });
    m
});

pub fn eval(node: &dyn Node, env: &Rc<RefCell<object::Environment>>) -> Option<Box<dyn Object>> {
    if let Some(program) = node.as_program() {
        return eval_program(&program.statements, env);
    } else if let Some(integer_literal) = node.as_any().downcast_ref::<IntegerLiteral>() {
        return Some(Box::new(object::Integer {
            value: integer_literal.value,
        }));
    } else if let Some(boolean) = node.as_any().downcast_ref::<Boolean>() {
        return Some(native_bool_to_boolean_object(boolean.value));
    } else if let Some(str_exp) = node.as_any().downcast_ref::<crate::ast::StringLiteral>() {
        return Some(Box::new(object::STRING {
            value: str_exp.value.clone(),
        }));
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
        return eval_infix_expression_wrap(infix_expr, env);
    } else if let Some(if_expr) = node.as_any().downcast_ref::<crate::ast::IfExpression>() {
        return eval_if_expression(if_expr, env);
    } else if let Some(block_stmt) = node.as_any().downcast_ref::<crate::ast::BlockStatement>() {
        return eval_block_statements(&block_stmt.statements, env);
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
        return eval_let_statement(let_stmt, env);
    } else if let Some(identifier) = node.as_any().downcast_ref::<crate::ast::Identifier>() {
        return eval_identifier(identifier, env);
    } else if let Some(func_node) = node.as_any().downcast_ref::<crate::ast::FunctionLiteral>() {
        let func_obj = object::Function::new(
            func_node.parameters.clone(),
            Box::new(func_node.body.clone()),
            env.clone(),
        );

        return Some(Box::new(func_obj));
    } else if let Some(call_expr) = node.as_any().downcast_ref::<crate::ast::CallExpression>() {
        let function = eval(call_expr.function.as_ref(), env);
        if is_error(&function) {
            return function;
        }

        let args = eval_expressions(&call_expr.arguments, env);
        if args.len() == 1 && is_error(&Some(args[0].clone())) {
            return args.first().cloned();
        }

        return apply_function(&function.unwrap(), args);
    } else if let Some(array_literal) = node.as_any().downcast_ref::<crate::ast::ArrayLiteral>() {
        let elements = eval_expressions(&array_literal.elements, env);
        if elements.len() == 1 && is_error(&Some(elements[0].clone())) {
            return elements.first().cloned();
        }

        return Some(Box::new(object::Array { elements }));
    } else if let Some(index_expr) = node.as_any().downcast_ref::<crate::ast::IndexExpression>() {
        let left = eval(index_expr.left.as_ref(), env);
        if is_error(&left) {
            return left;
        }

        let index = eval(index_expr.index.as_ref(), env);
        if is_error(&index) {
            return index;
        }

        return eval_index_expression(left, index);
    } else if let Some(hash) = node.as_any().downcast_ref::<crate::ast::HashLiteral>() {
        return eval_hash_literal(hash, env);
    }

    Some(Box::new(NULL_OBJ))
}

fn is_error(obj: &Option<Box<dyn Object>>) -> bool {
    match obj {
        None => false,
        Some(obj) => obj.as_error().is_some(),
    }
}

fn eval_program(
    statements: &[Box<dyn Statement>],
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    let mut object: Option<Box<dyn Object>> = None;

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

fn eval_block_statements(
    statements: &[Box<dyn Statement>],
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    let mut result: Option<Box<dyn Object>> = None;

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
        None => Some(native_bool_to_boolean_object(true)), // !null is true
        Some(obj) => {
            if let Some(b) = obj.as_boolean() {
                Some(native_bool_to_boolean_object(!b.value))
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
        return if let Some(i) = obj.as_integer() {
            Some(Box::new(object::Integer { value: -i.value }))
        } else {
            Some(new_error!("unknown operator: -{}", obj.type_name()))
        }
    }
    None
}

fn eval_infix_expression_wrap(
    infix_expr: &InfixExpression,
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    let left = eval(infix_expr.left.as_ref(), env);
    if is_error(&left) {
        return left;
    }

    let right = eval(infix_expr.right.as_ref(), env);
    if is_error(&right) {
        return right;
    }

    eval_infix_expression(&infix_expr.operator, left, right)
}

fn eval_infix_expression(
    operator: &str,
    left: Option<Box<dyn Object>>,
    right: Option<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    if let (Some(left_obj), Some(right_obj)) = (&left, &right) {
        if left_obj.type_name() == object::STRING_OBJ && right_obj.type_name() == object::STRING_OBJ
        {
            let left_str = left_obj.as_string().unwrap();
            let right_str = right_obj.as_string().unwrap();

            if operator != "+" {
                return Some(new_error!(
                    "unknown operator: {} {} {}",
                    left_obj.type_name(),
                    operator,
                    right_obj.type_name()
                ));
            }

            return Some(Box::new(object::STRING {
                value: format!("{}{}", left_str.value, right_str.value),
            }));
        }
    }

    match operator {
        "+" => eval_integer_infix_expression(left, right, |a, b| a + b, operator),
        "-" => eval_integer_infix_expression(left, right, |a, b| a - b, operator),
        "*" => eval_integer_infix_expression(left, right, |a, b| a * b, operator),
        "/" => eval_integer_infix_expression(left, right, |a, b| a / b, operator),
        "==" => {
            eval_boolean_infix_expression(left, right, |a, b| a == b, Some(|a, b| a == b), operator)
        }
        "!=" => {
            eval_boolean_infix_expression(left, right, |a, b| a != b, Some(|a, b| a != b), operator)
        }
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
        return if let (Some(left_int), Some(right_int)) = (left_obj.as_integer(), right_obj.as_integer()) {
            Some(Box::new(object::Integer {
                value: op(left_int.value, right_int.value),
            }))
        } else {
            Some(new_error!(
                "Type mismatch: {} {} {}",
                left_obj.type_name(),
                operator,
                right_obj.type_name()
            ))
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

fn eval_if_expression(
    ie: &crate::ast::IfExpression,
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    let condition = eval(ie.condition.as_ref(), env);
    if is_error(&condition) {
        return condition;
    }

    if let Some(cond_obj) = condition {
        return if if_truthy(&*cond_obj) {
            eval(&ie.consequence, env)
        } else if let Some(alternative) = &ie.alternative {
            eval(alternative, env)
        } else {
            Some(Box::new(NULL_OBJ))
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

fn eval_let_statement(
    let_stmt: &crate::ast::LetStatement,
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    let val_node = let_stmt.value.as_ref();
    let val = eval(val_node, env);
    if is_error(&val) {
        return val;
    }

    if let Some(value) = val {
        let v_copy = value.clone_box();
        env.borrow_mut().set(let_stmt.name.value.clone(), value);
        Some(v_copy)
    } else {
        Some(new_error!(
            "let statement value is None for identifier: {}",
            let_stmt.name.value
        ))
    }
}

fn eval_identifier(
    identifier: &crate::ast::Identifier,
    env: &Rc<RefCell<object::Environment>>,
) -> Option<Box<dyn Object>> {
    if let Some(value) = env.borrow().get(&identifier.value) {
        Some(value.clone_box())
    } else if let Some(builtin) = BUILTINS.get(identifier.value.as_str()) {
        Some(Box::new(builtin.clone()))
    } else {
        Some(new_error!("identifier not found: {}", identifier.value))
    }
}

fn eval_expressions(
    expressions: &[Box<dyn crate::ast::Expression>],
    env: &Rc<RefCell<object::Environment>>,
) -> Vec<Box<dyn Object>> {
    let mut args = Vec::with_capacity(expressions.len());

    for expr in expressions {
        let evaluated = eval(expr.as_ref(), env);
        if is_error(&evaluated) {
            return vec![evaluated.unwrap_or_else(|| Box::new(NULL_OBJ))];
        }

        if let Some(obj) = evaluated {
            args.push(obj);
        } else {
            args.push(Box::new(NULL_OBJ));
        }
    }

    args
}

fn eval_index_expression(
    left: Option<Box<dyn Object>>,
    index: Option<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    if let (Some(left_obj), Some(index_obj)) = (left, index) {
        return if let Some(array_obj) = left_obj.as_array() {
            if let Some(index_int) = index_obj.as_integer() {
                let idx = index_int.value;
                let max = array_obj.elements.len() as i64 - 1;

                if idx < 0 || idx > max {
                    return Some(Box::new(NULL_OBJ));
                }

                Some(array_obj.elements[idx as usize].clone_box())
            } else {
                Some(new_error!(
                    "index operator not supported: {}",
                    index_obj.type_name()
                ))
            }
        } else if let Some(hash_obj) = left_obj.as_hash() {
            let hashable_index = index_obj.as_hashable();
            if hashable_index.is_none() {
                return Some(new_error!(
                    "unusable as hash key: {}",
                    index_obj.type_name()
                ));
            }

            let hash_key = hashable_index.unwrap().hash_key();
            if let Some(hash_pair) = hash_obj.pairs.get(&hash_key) {
                Some(hash_pair.value.clone_box())
            } else {
                Some(Box::new(NULL_OBJ))
            }
        } else {
            Some(new_error!(
                "index operator not supported: {}",
                left_obj.type_name()
            ))
        };
    }
    None
}

fn eval_hash_literal(hash: &crate::ast::HashLiteral, env: &Rc<RefCell<object::Environment>>) -> Option<Box<dyn Object>> {
    let mut pairs: HashMap<object::HashKey, object::HashPair> = HashMap::new();

    for (key_node, value_node) in &hash.pairs {
        let key = eval(key_node.as_ref(), env);
        if is_error(&key) {
            return key;
        }

        let value = eval(value_node.as_ref(), env);
        if is_error(&value) {
            return value;
        }

        let key_obj = key.unwrap();
        let hashable_key = key_obj.as_hashable();
        if hashable_key.is_none() {
            return Some(new_error!(
                "unusable as hash key: {}",
                key_obj.type_name()
            ));
        }

        let hash_key = hashable_key.unwrap().hash_key();
        pairs.insert(hash_key, object::HashPair {
            key: key_obj,
            value: value.unwrap(),
        });
    }

    Some(Box::new(object::Hash { pairs }))

}

fn apply_function(
    function: &Box<dyn Object>,
    args: Vec<Box<dyn Object>>,
) -> Option<Box<dyn Object>> {
    // handle builtin functions
    if let Some(builtin) = function.as_builtin() {
        return (builtin.func)(args);
    }

    let func_opt = function.as_function();
    if func_opt.is_none() {
        return Some(new_error!("not a function: {}", function.type_name()));
    }

    let func = func_opt.unwrap();

    if func.parameters.len() != args.len() {
        return Some(new_error!(
            "wrong number of arguments: expected {}, got {}",
            func.parameters.len(),
            args.len()
        ));
    }

    let extended_env = extend_function_env(func, &args);
    let mut extended_env = extended_env.clone();
    let evaluated = eval(func.body.as_ref(), &mut extended_env);

    unwrap_return_value(evaluated)
}

fn unwrap_return_value(evaluated: Option<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if let Some(return_value) = evaluated {
        if return_value.type_name() == object::RETURN_VALUE_OBJ {
            return Some(return_value.as_return_value().unwrap().value.clone());
        }
        return Some(return_value);
    }
    Some(Box::new(NULL_OBJ)) // If no return value, return null
}

fn extend_function_env(
    function: &object::Function,
    args: &[Box<dyn Object>],
) -> Rc<RefCell<object::Environment>> {
    let extended_env = object::Environment::new_enclosed(function.env.clone());
    for (param, arg) in function.parameters.iter().zip(args.iter()) {
        extended_env
            .borrow_mut()
            .set(param.value.clone(), arg.clone_box());
    }
    extended_env
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::{Hashable, Object};
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
        let env = &object::Environment::new();

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
            Case {
                input: "return 10;",
                expected: Some(10),
            },
            Case {
                input: "return 10;9;",
                expected: Some(10),
            },
            Case {
                input: "return 20; return 30;",
                expected: Some(20),
            },
            Case {
                input: "if (true) { return 40; }",
                expected: Some(40),
            },
            Case {
                input: "if (false) { return 50; } else { return 60; }",
                expected: Some(60),
            },
            Case {
                input: "9; return 2 * 5; 10;",
                expected: Some(10),
            },
            Case {
                input: "let x = 5; return x;",
                expected: Some(5),
            },
            Case {
                input: "if (10 > 1) { if (true) { return 15; } return 1; } else { return 20; }",
                expected: Some(15),
            },
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
            Case {
                input: r#""Hello" - "World""#,
                expected_error: "unknown operator: String - String",
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
                input: "let x = 5; let c = x; c + c + 2;",
                expected: 12,
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
            Case {
                input: "let a = 5; let b = a > 3; if (b) { 10 } else {1};",
                expected: 10,
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            test_integer_object(object, test.expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) {x + 2;};";

        let object = test_eval(input);
        assert!(object.is_some(), "Expected an object, but got None");
        let object = object.unwrap();
        assert_eq!(object.type_name(), object::FUNCTION_OBJ);
        let function = object.as_function().unwrap();
        assert_eq!(function.parameters.len(), 1);
        assert_eq!(function.parameters[0].value, "x");
        assert_eq!(function.body.statements.len(), 1);
        let expect_body = "(x + 2)";
        assert_eq!(function.body.to_string(), expect_body);
    }

    #[test]
    fn test_function_application() {
        struct Case {
            input: &'static str,
            expected: i64,
        }

        let tests = vec![
            Case {
                input: "let identify = fn(x) { x; }; identify(5);",
                expected: 5,
            },
            Case {
                input: "let identify = fn(x) { return x; }; identify(5);",
                expected: 5,
            },
            Case {
                input: "let add = fn(x, y) { x + y; }; add(5, 10);",
                expected: 15,
            },
            Case {
                input: "let subtract = fn(x, y) { x - y; }; subtract(10, 5);",
                expected: 5,
            },
            Case {
                input: "let multiply = fn(x, y) { x * y; }; multiply(3, 4);",
                expected: 12,
            },
            Case {
                input: "let divide = fn(x, y) { x / y; }; divide(20, 4);",
                expected: 5,
            },
            Case {
                input: "let divide = fn(x, y) { x / y; }; divide(21 - 1, 2 + 2);",
                expected: 5,
            },
            Case {
                input: "fn(x) { x + 1; }(5);",
                expected: 6,
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            test_integer_object(object, test.expected);
        }
    }

    #[test]
    fn test_closure() {
        let input = r#"
        let newAdder = fn(x) {
            fn(y) { x + y; };
        };
        let addTwo = newAdder(2);
        addTwo(5);
        "#;
        let expected = 7;
        let object = test_eval(input);
        assert!(object.is_some(), "Expected an object, but got None");
        let object = object.unwrap();
        assert_eq!(object.type_name(), object::INTEGER_OBJ);
        let integer = object.as_integer().unwrap();
        assert_eq!(
            integer.value, expected,
            "Expected {}, got {}",
            expected, integer.value
        );
    }

    #[test]
    fn test_string_literal() {
        let input = r#"
        "Hello World!"
        "#;

        let evaluated = test_eval(input);

        let val = evaluated
            .as_ref()
            .unwrap()
            .as_string()
            .unwrap()
            .value
            .as_str();
        assert_eq!(val, "Hello World!");
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#"
        "Hello" + " " + "World!"
        "#;

        let evaluated = test_eval(input);
        let val = evaluated
            .as_ref()
            .unwrap()
            .as_string()
            .unwrap()
            .value
            .as_str();
        assert_eq!(val, "Hello World!");
    }

    #[test]
    fn test_builtin_function() {
        struct Case {
            input: &'static str,
            expected: Result<i64, &'static str>,
        }
        let input_list = vec![
            Case {
                input: r#"len("")"#,
                expected: Ok(0),
            },
            Case {
                input: r#"len("four")"#,
                expected: Ok(4),
            },
            Case {
                input: r#"len("hello world")"#,
                expected: Ok(11),
            },
            Case {
                input: r#"len(1)"#,
                expected: Err("argument to `len` not supported, got Integer"),
            },
            Case {
                input: r#"len("one", "two")"#,
                expected: Err("wrong number of arguments. got=2, want=1"),
            },
        ];

        for case in input_list {
            println!("Testing input: {}", case.input);

            let evaluated = test_eval(case.input);
            match case.expected {
                Ok(expected_int) => {
                    assert!(evaluated.is_some(), "Expected an object, but got None");
                    let evaluated = evaluated.unwrap();
                    println!("{:?}, {:?}", evaluated.type_name(), evaluated.inspect());
                    assert_eq!(evaluated.type_name(), object::INTEGER_OBJ);
                    let integer_obj = evaluated.as_integer().unwrap();
                    assert_eq!(
                        integer_obj.value, expected_int,
                        "Expected integer value {}, got {}",
                        expected_int, integer_obj.value
                    );
                }
                Err(expected_err) => {
                    assert!(evaluated.is_some(), "Expected an object, but got None");
                    let evaluated = evaluated.unwrap();
                    let error_obj = evaluated.as_error().unwrap();
                    assert_eq!(
                        error_obj.message, expected_err,
                        "Expected error message '{}', got '{}'",
                        expected_err, error_obj.message
                    );
                }
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input);
        assert!(evaluated.is_some(), "Expected an object, but got None");
        let evaluated = evaluated.unwrap();
        assert_eq!(evaluated.type_name(), object::ARRAY_OBJ);

        let array = evaluated.as_array().unwrap();
        assert_eq!(array.elements.len(), 3);

        test_integer_object(Some(array.elements[0].clone_box()), 1);
        test_integer_object(Some(array.elements[1].clone_box()), 4);
        test_integer_object(Some(array.elements[2].clone_box()), 6);
    }

    #[test]
    fn test_index_expressions() {
        struct Case {
            input: &'static str,
            expected: Option<i64>,
        }

        let tests = vec![
            Case {
                input: "[1, 2, 3][0]",
                expected: Some(1),
            },
            Case {
                input: "[1, 2, 3][1]",
                expected: Some(2),
            },
            Case {
                input: "[1, 2, 3][2]",
                expected: Some(3),
            },
            Case {
                input: "let i = 0; [1][i];",
                expected: Some(1),
            },
            Case {
                input: "[1, 2, 3][1 + 1];",
                expected: Some(3),
            },
            Case {
                input: "let myArray = [1, 2, 3]; myArray[2];",
                expected: Some(3),
            },
            Case {
                input: "[1, 2, 3][3];",
                expected: None,
            },
            Case {
                input: "[1, 2, 3][-1];",
                expected: None,
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            if let Some(expected_value) = test.expected {
                test_integer_object(object, expected_value);
            } else {
                assert!(object.is_some(), "Expected an object, but got None");
                let object = object.unwrap();
                test_null_object(&object);
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
        let two = "two";
        { "one": 10 - 9,
          two: 1 + 1,
          "thr" + "ee": 6 / 2,
          4: 4,
          true: 5,
          false: 6 }
        "#;

        let mut expected = HashMap::new();
        expected.insert(object::STRING{value: "one".to_string()}.hash_key(), 10 - 9);
        expected.insert(object::STRING{value: "two".to_string()}.hash_key(), 1+1);
        expected.insert(object::STRING{value: "three".to_string()}.hash_key(), 6/2);
        expected.insert(object::Integer{value: 4}.hash_key(), 4);
        expected.insert(object::Boolean{value: true}.hash_key(), 5);
        expected.insert(object::Boolean{value: false}.hash_key(), 6);

        let evaluated = test_eval(input);
        assert!(evaluated.is_some(), "Expected an object, but got None");
        let evaluated = evaluated.unwrap();

        assert_eq!(evaluated.type_name(), object::HASH_OBJ);
        let hash = evaluated.as_hash().unwrap();

        assert_eq!(hash.pairs.len(), expected.len());
        for (expected_key, expected_value) in expected {
            let pair = hash.pairs.get(&expected_key).unwrap();
            test_integer_object(Some(pair.value.clone_box()), expected_value);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        struct Case {
            input: &'static str,
            expected: Option<i64>,
        }
        let tests = vec![
            Case {
                input: r#"{ "foo": 5 }["foo"]"#,
                expected: Some(5),
            },
            Case {
                input: r#"{ "foo": 5 }["bar"]"#,
                expected: None,
            },
            Case {
                input: r#"let key = "foo"; { "foo": 5 }[key]"#,
                expected: Some(5),
            },
            Case {
                input: r#"{ 5: 5 }[5]"#,
                expected: Some(5),
            },
            Case {
                input: r#"{ true: 5 }[true]"#,
                expected: Some(5),
            },
            Case {
                input: r#"{ false: 5 }[false]"#,
                expected: Some(5),
            },
        ];

        for test in tests {
            println!("Testing input: {}", test.input);
            let object = test_eval(test.input);
            if let Some(expected_value) = test.expected {
                test_integer_object(object, expected_value);
            } else {
                assert!(object.is_some(), "Expected an object, but got None");
                let object = object.unwrap();
                test_null_object(&object);
            }
        }
    }

    #[test]
    fn test_error_handling_for_index() {
        let input = r#"{ "name": "Monkey" }[fn(x) { x }];"#;

        let evaluated = test_eval(input);
        assert!(evaluated.is_some(), "Expected an object, but got None");
    }
}

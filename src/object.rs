// Object types

use crate::ast;
use std::cell::RefCell;
use std::fmt::Write;
use std::rc::Rc;

pub const INTEGER_OBJ: &str = "Integer";
pub const BOOLEAN_OBJ: &str = "Boolean";
pub const NULL_OBJ: &str = "Null";
pub const RETURN_VALUE_OBJ: &str = "ReturnValue";
pub const ERROR_OBJ: &str = "Error";
pub const FUNCTION_OBJ: &str = "Function";
pub const STRING_OBJ: &str = "String";
pub const BUILTIN_OBJ: &str = "Builtin";
pub const ARRAY_OBJ: &str = "Array";
pub const HASH_OBJ: &str = "Hash";

type ObjectType = String;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct HashKey {
    pub tp: ObjectType,
    pub value: u64,
}

pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

pub trait Object {
    fn type_name(&self) -> ObjectType;

    /// Returns a string representation of the object.
    fn inspect(&self) -> String;
    fn clone_box(&self) -> Box<dyn Object>;

    fn as_integer(&self) -> Option<Integer> {
        None
    }
    fn as_boolean(&self) -> Option<Boolean> {
        None
    }
    fn as_return_value(&self) -> Option<&ReturnValue> {
        None
    }
    fn as_error(&self) -> Option<&Error> {
        None
    }
    fn as_function(&self) -> Option<&Function> {
        None
    }
    fn as_string(&self) -> Option<&STRING> {
        None
    }
    fn as_builtin(&self) -> Option<&Builtin> {
        None
    }
    fn as_array(&self) -> Option<&Array> {None}
    fn as_hash(&self) -> Option<&Hash> {None}
    fn as_hashable(&self) -> Option<&dyn Hashable> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn type_name(&self) -> ObjectType {
        INTEGER_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
    fn as_integer(&self) -> Option<Integer> {
        Some(self.clone())
    }
    fn as_hashable(&self) -> Option<&dyn Hashable> {
        Some(self)
    }
}

impl Hashable for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey {
            tp: self.type_name(),
            value: self.value as u64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn type_name(&self) -> ObjectType {
        BOOLEAN_OBJ.into()
    }
    fn inspect(&self) -> String {
        self.value.to_string()
    }
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
    fn as_boolean(&self) -> Option<Boolean> {
        Some(self.clone())
    }
    fn as_hashable(&self) -> Option<&dyn Hashable> {
        Some(self)
    }
}

impl Hashable for Boolean {
    fn hash_key(&self) -> HashKey {
        let value = if self.value { 1 } else { 0 };
        HashKey {
            tp: self.type_name(),
            value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Null;

impl Object for Null {
    fn type_name(&self) -> ObjectType {
        NULL_OBJ.into()
    }
    fn inspect(&self) -> String {
        "null".into()
    }
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

pub struct ReturnValue {
    pub value: Box<dyn Object>,
}

impl Object for ReturnValue {
    fn type_name(&self) -> ObjectType {
        RETURN_VALUE_OBJ.into()
    }

    fn inspect(&self) -> String {
        format!("ReturnValue({})", self.value.inspect())
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_return_value(&self) -> Option<&ReturnValue> {
        Some(self)
    }
}

impl Clone for ReturnValue {
    fn clone(&self) -> ReturnValue {
        ReturnValue {
            value: self.value.clone_box(),
        }
    }
}

impl Clone for Box<dyn Object> {
    fn clone(&self) -> Box<dyn Object> {
        self.clone_box()
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn type_name(&self) -> ObjectType {
        ERROR_OBJ.into()
    }

    fn inspect(&self) -> String {
        format!("Error: {}", self.message)
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_error(&self) -> Option<&Error> {
        Some(self)
    }
}

#[derive(Clone)]
pub struct Environment {
    pub store: std::collections::HashMap<String, Box<dyn Object>>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        let env = Environment {
            store: std::collections::HashMap::new(),
            outer: None,
        };

        Rc::new(RefCell::new(env))
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        let env = Environment {
            store: std::collections::HashMap::new(),
            outer: Some(outer),
        };

        Rc::new(RefCell::new(env))
    }

    pub fn set(&mut self, name: String, value: Box<dyn Object>) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Box<dyn Object>> {
        match self.store.get(name) {
            Some(value) => Some(value.clone_box()),
            None => {
                if let Some(outer) = &self.outer {
                    outer.borrow().get(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn remove(&mut self, name: &str) -> Option<Box<dyn Object>> {
        self.store.remove(name)
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: Box<ast::BlockStatement>,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        parameters: Vec<ast::Identifier>,
        body: Box<ast::BlockStatement>,
        env: Rc<RefCell<Environment>>,
    ) -> Self {
        Function {
            parameters,
            body,
            env,
        }
    }
}

impl Object for Function {
    fn type_name(&self) -> ObjectType {
        FUNCTION_OBJ.into()
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        out.write_str("fn(").unwrap();
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                out.write_str(", ").unwrap();
            }
            out.write_str(&param.to_string()).unwrap();
        }

        out.write_str(") {\n").unwrap();
        out.write_str(self.body.to_string().as_str()).unwrap();
        out.write_str("\n}").unwrap();

        out.to_string()
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_function(&self) -> Option<&Function> {
        Some(self)
    }
}

#[derive(Clone)]
pub struct STRING {
    pub value: String,
}

impl Object for STRING {
    fn type_name(&self) -> ObjectType {
        STRING_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        self.value.clone()
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_string(&self) -> Option<&STRING> {
        Some(self)
    }
    fn as_hashable(&self) -> Option<&dyn Hashable> {
        Some(self)
    }
}

impl Hashable for STRING {
    fn hash_key(&self) -> HashKey {
        let mut hash: u64 = 5381;
        for byte in self.value.as_bytes() {
            hash = ((hash << 5).wrapping_add(hash)).wrapping_add(*byte as u64);
        }
        HashKey {
            tp: self.type_name(),
            value: hash,
        }
    }
}

pub type BuiltinFunction = fn(Vec<Box<dyn Object>>) -> Option<Box<dyn Object>>;

#[derive(Clone)]
pub struct Builtin {
    pub func: BuiltinFunction,
}

impl Object for Builtin {
    fn type_name(&self) -> ObjectType {
        BUILTIN_OBJ.into()
    }

    fn inspect(&self) -> String {
        "builtin function".into()
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
    fn as_builtin(&self) -> Option<&Builtin> {
        Some(self)
    }
}

#[derive(Clone)]
pub struct Array {
    pub elements: Vec<Box<dyn Object>>,
}

impl Object for Array {
    fn type_name(&self) -> ObjectType {
        ARRAY_OBJ.into()
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        out.push('[');
        let elements: Vec<String> = self.elements.iter().
            map(|e| e.inspect()).collect();
        out.push_str(&elements.join(", "));
        out.push(']');
        out
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_array(&self) -> Option<&Array> {
        Some(self)
    }
}

#[derive(Clone)]
pub struct HashPair {
    pub key: Box<dyn Object>,
    pub value: Box<dyn Object>,
}

#[derive(Clone)]
pub struct Hash {
    pub pairs: std::collections::HashMap<HashKey, HashPair>,
}

impl Object for Hash {
    fn type_name(&self) -> ObjectType {
        HASH_OBJ.into()
    }

    fn inspect(&self) -> String {
        let mut out = String::new();
        out.push('{');
        let mut pairs: Vec<String> = Vec::new();
        for pair in self.pairs.values() {
            let pair_str = format!("{}: {}", pair.key.inspect(), pair.value.inspect());
            pairs.push(pair_str);
        }
        out.push_str(&pairs.join(", "));
        out.push('}');
        out
    }

    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }

    fn as_hash(&self) -> Option<&Hash> {
        Some(self)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let s1 = STRING {
            value: "Hello World".to_string(),
        };
        let s2 = STRING {
            value: "Hello World".to_string(),
        };
        let diff1 = STRING {
            value: "My name is johnny".to_string(),
        };
        let diff2 = STRING {
            value: "My name is johnny".to_string(),
        };

        if s1.hash_key() == s2.hash_key() {
            assert!(true);
        } else {
            assert!(false, "strings with same content have different hash keys");
        }

        if diff1.hash_key() == diff2.hash_key() {
            assert!(true);
        } else {
            assert!(false, "strings with same content have different hash keys");
        }

        assert_ne!(s1.hash_key(), diff1.hash_key());
    }
}
// Object types

const INTEGER_OBJ: &str = "Integer";
const BOOLEAN_OBJ: &str = "Boolean";
const NULL_OBJ: &str = "Null";

type ObjectType = String;

pub trait Object {
    fn type_name(&self) -> ObjectType;

    /// Returns a string representation of the object.
    fn inspect(&self) -> String;
}

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
}

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
}

pub struct Null;

impl Object for Null {
    fn type_name(&self) -> ObjectType {
        NULL_OBJ.into()
    }

    fn inspect(&self) -> String {
        "null".into()
    }
}
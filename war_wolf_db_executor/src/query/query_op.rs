use std::{
    cell::{Ref, RefCell},
    fmt::Display,
    rc::{Rc, Weak},
};

use super::operator::LogicOp;

#[derive(Debug, PartialEq, Clone)]
pub enum Column {
    TableColumn(TableColumn),
    FuncColumn(FuncColumn),
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TableColumn {
    pub(crate) table_name: String,
    pub(crate) column: String,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct FuncColumn {
    pub(crate) func_name: String,
    pub(crate) args: Vec<TableColumn>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum QueryType {
    #[default]
    Select,
    Insert,
    Update,
    Delete,
}

#[derive(Default, Debug, Clone)]
pub struct QueryOp {
    pub(crate) data: LogicOp,
    pub(crate) children: Vec<Rc<RefCell<QueryOp>>>,
}

impl PartialEq for QueryOp {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.children == self.children
    }
}

impl QueryOp {
    pub fn new(data: LogicOp) -> Self {
        QueryOp {
            data,
            children: vec![],
        }
    }

    pub fn add_child(&mut self, child: LogicOp) -> Rc<RefCell<QueryOp>> {
        let child = Rc::new(RefCell::new(QueryOp {
            data: child,
            children: vec![],
        }));

        self.children.push(Rc::clone(&child));

        child
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_query_tree() {}
}

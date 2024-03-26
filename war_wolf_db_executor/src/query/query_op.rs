use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use super::operator::LogicOp;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TableColumn {
    pub(crate) table_name: String,
    pub(crate) column: String,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub enum QueryType {
    #[default]
    Select,
    Insert,
    Update,
    Delete,
}

// tree root node
#[derive(Default, Debug, PartialEq, Clone)]
pub struct Query {
    pub(crate) query_type: QueryType,
    pub(crate) root: Option<Rc<RefCell<QueryOp>>>,
    pub(crate) tail: Option<Rc<RefCell<QueryOp>>>,
}

#[derive(Default, Debug, Clone)]
pub struct QueryOp {
    pub(crate) data: LogicOp,
    pub(crate) parent: Option<Weak<RefCell<QueryOp>>>,
    pub(crate) next: Option<Rc<RefCell<QueryOp>>>,
}

impl PartialEq for QueryOp {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.next == self.next
    }
}

impl Query {
    fn new() -> Self {
        todo!()
    }

    fn add_child(&mut self, child: LogicOp) {
        todo!()
    }
}

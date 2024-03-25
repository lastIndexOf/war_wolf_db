use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use super::operator::LogicOp;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TableColumn {
    table_name: String,
    column: String,
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
    pub(crate) root: Option<Rc<RefCell<QueryNode>>>,
    pub(crate) tail: Option<Rc<RefCell<QueryNode>>>,
}

#[derive(Default, Debug, Clone)]
pub struct QueryNode {
    pub(crate) data: LogicOp,
    pub(crate) parent: Option<Weak<RefCell<QueryNode>>>,
    pub(crate) next: Option<Rc<RefCell<QueryNode>>>,
}

impl PartialEq for QueryNode {
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

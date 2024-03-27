use std::{
    cell::RefCell,
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

// tree root node
#[derive(Default, Debug, PartialEq, Clone)]
pub struct Query {
    pub(crate) query_type: QueryType,
    pub(crate) root: Option<Rc<RefCell<QueryOp>>>,
    pub(crate) tail: Option<Rc<RefCell<QueryOp>>>,
    pub(crate) size: usize,
}

#[derive(Default, Debug, Clone)]
pub struct QueryOp {
    pub(crate) data: LogicOp,
    // pub(crate) parent: Option<Weak<RefCell<QueryOp>>>,
    pub(crate) next: Option<Rc<RefCell<QueryOp>>>,
}

impl PartialEq for QueryOp {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.next == self.next
    }
}

impl Query {
    pub fn new() -> Self {
        Query {
            query_type: QueryType::Select,
            root: None,
            tail: None,
            size: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn get_n(&self, n: usize) -> Option<LogicOp> {
        let mut cur = self.root.as_ref().map(Rc::clone);

        for _ in 0..n {
            if let Some(node) = cur {
                cur = node.borrow().next.as_ref().map(Rc::clone);
            } else {
                return None;
            }
        }

        cur.map(|ele| ele.borrow().data.clone())
    }

    pub fn add_child(&mut self, child: LogicOp) {
        let new_node = Rc::new(RefCell::new(QueryOp {
            data: child,
            // parent: None,
            next: None,
        }));

        if self.root.is_none() {
            // empty tree
            self.root = Some(Rc::clone(&new_node));
            self.tail = Some(Rc::clone(&new_node));
        } else {
            // non-empty tree
            let tail = self.tail.take().unwrap();
            tail.borrow_mut().next = Some(Rc::clone(&new_node));
            self.tail = Some(Rc::clone(&new_node));
        }

        self.size += 1;
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret = String::new();

        let mut current = self.root.as_ref().map(|ele| Rc::clone(ele));

        while let Some(node) = current {
            ret = format!("{} - {:?}\n", ret, node.borrow().data);
            current = node.borrow().next.as_ref().map(|ele| Rc::clone(ele));
        }

        write!(f, "{}", ret)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_query_tree() {
        let mut query = Query::new();
        query.add_child(LogicOp::Scan(Default::default()));
        query.add_child(LogicOp::Condition);
        query.add_child(LogicOp::Filter);
        query.add_child(LogicOp::Sort);
        query.add_child(LogicOp::Group);
        query.add_child(LogicOp::Join);
        query.add_child(LogicOp::Insert);
        query.add_child(LogicOp::Delete);
        query.add_child(LogicOp::Update);
        query.add_child(LogicOp::Ddl);

        assert_eq!(query.get_n(2), Some(LogicOp::Filter));
        assert_eq!(query.len(), 10);
    }
}

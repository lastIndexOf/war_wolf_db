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

// tree root node
#[derive(Default, Debug, PartialEq, Clone)]
pub struct Query {
    pub(crate) query_type: QueryType,
    pub(crate) ops: Vec<Rc<RefCell<QueryOp>>>,
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
    pub fn add_child(&mut self, child: LogicOp) -> &Rc<RefCell<QueryOp>> {
        let child = Rc::new(RefCell::new(QueryOp {
            data: child,
            children: vec![],
        }));

        self.children.push(child);

        self.children.last().unwrap()
    }
}

impl Query {
    pub fn new() -> Self {
        Query {
            query_type: QueryType::Select,
            ops: vec![],
        }
    }

    pub fn add_child(&mut self, child: LogicOp) -> &Rc<RefCell<QueryOp>> {
        let child = Rc::new(RefCell::new(QueryOp {
            data: child,
            children: vec![],
        }));

        self.ops.push(child);

        self.ops.last().unwrap()
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: implement BFS
        let mut ret = String::new();

        for op in &self.ops {
            ret.push_str(&format!("- {:?}", op.borrow().data));
            ret.push_str("\n");
            for child in &op.borrow().children {
                ret.push_str(&format!(" - {:?}", child.borrow().data));
                ret.push_str("\n");
            }
        }

        write!(f, "{}", ret)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_query_tree() {
        let query = {
            let mut query = Query::new();
            let cur = Rc::clone(query.add_child(LogicOp::Scan(Default::default())));
            let cur = Rc::clone(cur.borrow_mut().add_child(LogicOp::Condition));
            cur.borrow_mut()
                .add_child(LogicOp::Scan(Default::default()));
            cur.borrow_mut()
                .add_child(LogicOp::Scan(Default::default()));
            cur.borrow_mut()
                .add_child(LogicOp::Scan(Default::default()));
            let cur = Rc::clone(cur.borrow_mut().add_child(LogicOp::Filter));
            cur.borrow_mut()
                .add_child(LogicOp::Scan(Default::default()));

            query
        };

        println!("{}", query);
        // assert_eq!(sort, RefCell::new(LogicOp::Sort));
    }
}

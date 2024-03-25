use std::{
    cell::RefCell,
    fmt::Debug,
    rc::{Rc, Weak},
};

use war_wolf_db_metadata::table::Column;

use crate::parser::ast::{self, Infix, JoinType};

#[derive(Default, Debug, PartialEq, Clone)]
pub enum LogicOp {
    Scan(Scan),
    Condition,
    Filter,
    Sort,
    Group,
    Join,
    Insert,
    Delete,
    Update,
    Ddl,
    #[default]
    Null,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct QueryBuilder {
    project_columns: Vec<TableColumn>,
    scan_operators: Vec<Scan>,
    filter_operators: Vec<Filter>,
    join_operator: Join,
    order_operator: Order,
    group_columns: Vec<TableColumn>,
}

impl QueryBuilder {
    pub fn new() -> Self {
        todo!()
    }

    /// return operator
    pub fn build(self) -> LogicOp {
        todo!()
    }

    pub fn with_select_stmt(self, stmt: ast::Stmt) -> Self {
        assert!(matches!(stmt, ast::Stmt::SelectStmt { .. }));

        let mut this = self
            .with_from_clause()
            .with_select_clause()
            .with_where_clause()
            .with_join_clause()
            .with_order_clause()
            .with_group_by_clause();

        this.optimize();

        this
    }

    fn with_from_clause(self) -> Self {
        todo!()
    }

    fn with_select_clause(self) -> Self {
        todo!()
    }

    fn with_where_clause(self) -> Self {
        todo!()
    }

    fn with_join_clause(self) -> Self {
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    fn with_order_clause(self) -> Self {
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    fn with_group_by_clause(self) -> Self {
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    fn optimize(&mut self) -> &mut Self {
        todo!()
    }
}

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
#[derive(Default, Debug, Clone)]
pub struct Query {
    query_type: QueryType,
    root: Option<Rc<RefCell<QueryNode>>>,
    tail: Option<Rc<RefCell<QueryNode>>>,
}

#[derive(Default, Debug, Clone)]
pub struct QueryNode {
    data: LogicOp,
    parent: Option<Weak<RefCell<QueryNode>>>,
    next: Option<Rc<RefCell<QueryNode>>>,
}

impl Query {
    fn new() -> Self {
        todo!()
    }

    fn add_child(&mut self, child: LogicOp) {
        todo!()
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Scan {
    table_name: String,
    columns: Vec<Column>,
}

#[derive(Default, Debug, PartialEq, Clone)]
enum CondVal {
    Column(TableColumn),
    Literal(String),
    Number(i64),
    #[default]
    Null,
}

#[derive(Default, Debug, PartialEq, Clone)]
struct Condition {
    sign: Infix,
    left: CondVal,
    right: CondVal,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Filter {
    conditions: Vec<Condition>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Join {
    join_type: JoinType,
    left_table_name: String,
    right_table_name: String,
    condition: Condition,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Order {
    column: TableColumn,
    order: ast::Ordering,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct GroupBy {
    columns: Vec<TableColumn>,
    aggregate_fn: String,
    aggregate_column: String,
}

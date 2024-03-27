use std::fmt::Debug;

use war_wolf_db_metadata::table::Column;
use war_wolf_db_sql::parser::ast::{self, Infix, JoinType};

use super::query_op::TableColumn;

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

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Scan {
    pub(crate) table_name: String,
    pub(crate) columns: Vec<Column>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub(crate) enum CondVal {
    Column(TableColumn),
    Literal(String),
    #[default]
    Null,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Condition {
    pub(crate) sign: Infix,
    pub(crate) left: CondVal,
    pub(crate) right: CondVal,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Filter {
    pub(crate) conditions: Vec<Condition>,
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

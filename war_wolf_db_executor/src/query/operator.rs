use std::fmt::Debug;

use war_wolf_db_metadata::table::Column;
use war_wolf_db_sql::parser::ast::{self, Expr, Infix, JoinType};

use super::query_op::{FuncColumn, TableColumn};

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

impl From<&Expr> for Condition {
    fn from(expr: &Expr) -> Self {
        match expr {
            ast::Expr::InfixExpr(infix, left, right) => {
                match (left.as_ref(), right.as_ref()) {
                    (ast::Expr::DotExpr(lp, lc), ast::Expr::DotExpr(rp, rc)) => {
                        match (lp.as_ref(), lc.as_ref(), rp.as_ref(), rc.as_ref()) {
                            (
                                ast::Expr::IdentExpr(lpt),
                                ast::Expr::IdentExpr(lct),
                                ast::Expr::IdentExpr(rpt),
                                ast::Expr::IdentExpr(rct),
                            ) => {
                                let cond = Condition {
                                    sign: infix.clone(),
                                    left: CondVal::Column(TableColumn {
                                        table_name: lpt.to_string(),
                                        column: lct.to_string(),
                                    }),
                                    right: CondVal::Column(TableColumn {
                                        table_name: rpt.to_string(),
                                        column: rct.to_string(),
                                    }),
                                };

                                return cond;
                            }
                            _ => {
                                // TODO: add custom error
                                panic!("Invalid expression in where clause: {:?}. Only column expressions are allowed", expr);
                            }
                        }
                    }
                    (ast::Expr::DotExpr(tb, col), ast::Expr::LiteralExpr(literal))
                    | (ast::Expr::LiteralExpr(literal), ast::Expr::DotExpr(tb, col)) => {
                        match (tb.as_ref(), col.as_ref()) {
                            (ast::Expr::IdentExpr(tb), ast::Expr::IdentExpr(col)) => {
                                let cond = Condition {
                                    sign: infix.clone(),
                                    left: CondVal::Column(TableColumn {
                                        table_name: tb.to_string(),
                                        column: col.to_string(),
                                    }),
                                    right: CondVal::Literal(literal.to_string()),
                                };

                                return cond;
                            }
                            _ => {
                                // TODO: add custom error
                                panic!("Invalid expression in where clause: {:?}. Only column expressions are allowed", expr);
                            }
                        }
                    }
                    (ast::Expr::IdentExpr(left), ast::Expr::IdentExpr(right)) => {
                        unimplemented!()
                    }
                    (ast::Expr::IdentExpr(ident), ast::Expr::LiteralExpr(literal))
                    | (ast::Expr::LiteralExpr(literal), ast::Expr::IdentExpr(ident)) => {
                        unimplemented!()
                    }
                    (ast::Expr::IdentExpr(ident), ast::Expr::DotExpr(left, right))
                    | (ast::Expr::DotExpr(left, right), ast::Expr::IdentExpr(ident)) => {
                        unimplemented!()
                    }
                    _ => {
                        // TODO: add custom error
                        panic!("Invalid expression in where clause: {:?}. Only column expressions are allowed", expr);
                    }
                }
            }
            _ => {
                // TODO: add custom error
                panic!(
                    "Invalid expression in where clause: {:?}. Only column expressions are allowed",
                    expr
                );
            }
        }
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Filter {
    pub(crate) conditions: Vec<Condition>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Join {
    pub(crate) join_type: JoinType,
    pub(crate) left_table_name: String,
    pub(crate) right_table_name: String,
    pub(crate) condition: Vec<Condition>,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Order {
    pub(crate) column: TableColumn,
    pub(crate) order: ast::Ordering,
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct GroupBy {
    pub(crate) column: TableColumn,
    pub(crate) aggregate_fn: Option<String>,
    pub(crate) aggregate_column: Option<TableColumn>,
}

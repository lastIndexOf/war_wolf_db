use war_wolf_db_metadata::{table::columns_exists, Metadata, TABLE};
use war_wolf_db_sql::parser::ast::{self, Clause, Ident, Literal};

use self::{
    operator::{Filter, Join, Order, Scan},
    query_op::{Query, TableColumn},
};

pub mod operator;
mod query_op;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct QueryBuilder {
    project_columns: Vec<TableColumn>,
    scan_operators: Vec<Scan>,
    filter_operators: Vec<Filter>,
    join_operator: Option<Join>,
    order_operator: Option<Order>,
    group_columns: Vec<TableColumn>,
}

impl QueryBuilder {
    pub fn new() -> Self {
        todo!()
    }

    pub fn build(self) -> Query {
        todo!()
    }

    pub fn with_stmt(self, stmt: &ast::Stmt) -> Self {
        // assert!(matches!(stmt, ast::Stmt::SelectStmt { .. }));

        let mut this = self;

        match stmt {
            ast::Stmt::SelectStmt {
                select,
                from,
                condition,
                ordering,
                group_by,
            } => {
                this = this.with_from_clause(from);
                this = this.with_select_clause(select);

                if let Some(clause) = condition {
                    this = this.with_where_clause(clause);
                }

                if let Some(clause) = ordering {
                    this = this.with_order_clause(clause);
                }

                if let Some(clause) = group_by {
                    this = this.with_group_by_clause(clause);
                }
            }
            _ => todo!(),
        }

        this.optimize();

        this
    }

    #[inline]
    fn with_from_clause(mut self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::FromClause(_, _)));

        let mut unchecked_tables = vec![];
        let mut checked_tables = vec![];

        match clause {
            Clause::FromClause(name, join_clause) => {
                unchecked_tables.push(name.to_string());

                if let Some(join_clause) = join_clause {
                    // handle join clause
                    if let Clause::JoinClause { join_on, .. } = join_clause.as_ref() {
                        unchecked_tables.push(join_on.to_string());
                    }
                }
            }
            _ => {}
        }

        for table_name in unchecked_tables {
            // check if table exists
            // if table exists, add to checked_tables
            // else, return error
            let rets = TABLE
                .get()
                .unwrap()
                .select(|table| table.name == table_name);

            if rets.is_empty() {
                // TODO: add custom error
                panic!("Table {} does not exist", table_name);
            }

            checked_tables.push(table_name);
        }

        for table_name in checked_tables {
            let table = TABLE
                .get()
                .unwrap()
                .select(|table| table.name == table_name)[0];

            let scan = Scan {
                table_name: table.name.clone(),
                columns: table.columns.clone(),
            };

            self.scan_operators.push(scan);
        }

        self
    }

    #[inline]
    fn with_select_clause(mut self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::SelectClause(_)));

        let mut table_cols = vec![];

        match clause {
            Clause::SelectClause(exprs) => {
                for expr in exprs {
                    match expr {
                        ast::Expr::LiteralExpr(Literal::Star) => {
                            if !table_cols.is_empty() {
                                // TODO: add custom error
                                panic!("* cannot be used with other columns");
                            }

                            for scan in &self.scan_operators {
                                let table = TABLE
                                    .get()
                                    .unwrap()
                                    .select(|table| table.name == scan.table_name)[0];

                                for column in &table.columns {
                                    table_cols.push(TableColumn {
                                        table_name: table.name.clone(),
                                        column: column.name.clone(),
                                    });
                                }
                            }

                            break;
                        }
                        ast::Expr::IdentExpr(Ident(ident)) => {
                            if ident.contains('.') {
                                let parts: Vec<&str> = ident.split('.').collect();
                                let table_name = parts[0];
                                let column_name = parts[1];

                                if !columns_exists(&TABLE.get().unwrap(), table_name, column_name) {
                                    // TODO: add custom error
                                    panic!(
                                        "Column {} does not exist in table {}",
                                        column_name, table_name
                                    );
                                }

                                table_cols.push(TableColumn {
                                    table_name: table_name.to_string(),
                                    column: column_name.to_string(),
                                });
                            } else {
                                let mut founded = false;

                                for scan in &self.scan_operators {
                                    let table_md = TABLE.get().unwrap();

                                    if columns_exists(table_md, &scan.table_name, &ident) {
                                        if founded {
                                            // TODO: add custom error
                                            panic!("Column {} is ambiguous", ident);
                                        }

                                        table_cols.push(TableColumn {
                                            table_name: scan.table_name.clone(),
                                            column: ident.clone(),
                                        });

                                        founded = true;
                                    }
                                }

                                if !founded {
                                    // TODO: add custom error
                                    panic!(
                                        "Column {} does not exist in tables {}",
                                        ident,
                                        self.scan_operators
                                            .iter()
                                            .map(|scan| { &scan.table_name[..] })
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    );
                                }
                            }
                        }
                        _ => {
                            // TODO: add custom error
                            panic!("Invalid expression in select clause: {:?}", expr);
                        }
                    }
                }
            }
            _ => {}
        }

        self.project_columns = table_cols;

        self
    }

    #[inline]
    fn with_where_clause(self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::WhereClause(_)));

        match clause {
            Clause::WhereClause(exprs) => {
                let mut filters = Filter::default();

                for expr in exprs {}
            }
            _ => {}
        }

        self
    }

    #[inline]
    fn with_join_clause(self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::JoinClause { .. }));
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    #[inline]
    fn with_order_clause(self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::OrderByClause(_)));
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    #[inline]
    fn with_group_by_clause(self, clause: &Clause) -> Self {
        assert!(matches!(clause, Clause::GroupByClause(_)));
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    fn optimize(&mut self) {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use war_wolf_db_sql::{
        lexer::{token::Tokens, Lexer},
        parser::Parser,
    };

    use crate::query::QueryBuilder;

    use super::{
        operator::LogicOp,
        query_op::{Query, QueryOp, QueryType},
    };

    fn compare_input_with_query(input: &str, expected: Query) {
        let tokens = Lexer::lex(input).unwrap();
        let tokens = Tokens::new(&tokens);
        let ast = Parser::parse(tokens).unwrap();
        let query = QueryBuilder::new().with_stmt(&ast[0]).build();

        assert_eq!(query, expected);
    }

    #[test]
    fn test_query_builder() {
        let input = "select t1.name, t2.age from t1;";
        let expected = Query {
            query_type: QueryType::Select,
            root: Some(Rc::new(RefCell::new(QueryOp {
                data: LogicOp::Scan(super::Scan {
                    table_name: "t1".to_string(),
                    columns: vec![],
                }),
                parent: None,
                next: None,
            }))),
            tail: None,
        };
        compare_input_with_query(input, expected);
    }
}

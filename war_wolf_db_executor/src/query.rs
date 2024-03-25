use war_wolf_db_sql::parser::ast;

use self::{
    operator::{Filter, Join, Order, Scan},
    query_list::{Query, TableColumn},
};

pub mod operator;
mod query_list;

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

    #[inline]
    fn with_from_clause(self) -> Self {
        todo!()
    }

    #[inline]
    fn with_select_clause(self) -> Self {
        todo!()
    }

    #[inline]
    fn with_where_clause(self) -> Self {
        todo!()
    }

    #[inline]
    fn with_join_clause(self) -> Self {
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    #[inline]
    fn with_order_clause(self) -> Self {
        // select t1.name, t2.age from t1 left join t2 on t1.id = t2.uid;
        todo!()
    }

    #[inline]
    fn with_group_by_clause(self) -> Self {
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
        query_list::{Query, QueryNode, QueryType},
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
            root: Some(Rc::new(RefCell::new(QueryNode {
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

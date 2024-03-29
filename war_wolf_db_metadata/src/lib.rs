use std::sync::OnceLock;

use func::{Func, FuncMD};
use table::{Column, DataType, Table, TableMD};

pub mod builtin;
pub mod database;
pub mod func;
pub mod index;
pub mod table;

pub static TABLE: OnceLock<TableMD> = OnceLock::new();
pub static FUNC: OnceLock<FuncMD> = OnceLock::new();

pub fn init() {
    TABLE.get_or_init(|| {
        let mut table = TableMD::new("mock table");

        table.insert_many(vec![
            Table {
                name: "t1".to_owned(),
                columns: vec![
                    Column {
                        name: "id".to_owned(),
                        data_type: DataType::Int,
                    },
                    Column {
                        name: "name".to_owned(),
                        data_type: DataType::String,
                    },
                ],
            },
            Table {
                name: "t2".to_owned(),
                columns: vec![
                    Column {
                        name: "id".to_owned(),
                        data_type: DataType::Int,
                    },
                    Column {
                        name: "name".to_owned(),
                        data_type: DataType::String,
                    },
                    Column {
                        name: "age".to_owned(),
                        data_type: DataType::Int,
                    },
                ],
            },
        ]);

        table
    });

    FUNC.get_or_init(|| {
        let mut func = FuncMD::new("mock func");

        func.insert_many(vec![
            Func {
                func_name: "count".into(),
                args_num: 1,
                callback: Box::new(|inputs: Vec<String>| 1),
                is_aggregate: true,
            },
            Func {
                func_name: "sum".into(),
                args_num: 1,
                callback: Box::new(|inputs: Vec<String>| 1),
                is_aggregate: true,
            },
            Func {
                func_name: "max".into(),
                args_num: 1,
                callback: Box::new(|inputs: Vec<String>| 1),
                is_aggregate: true,
            },
            Func {
                func_name: "min".into(),
                args_num: 1,
                callback: Box::new(|inputs: Vec<String>| 1),
                is_aggregate: true,
            },
            Func {
                func_name: "avg".into(),
                args_num: 1,
                callback: Box::new(|inputs: Vec<String>| 1),
                is_aggregate: true,
            },
        ]);

        func
    });
}

pub trait Metadata {
    type Data;

    fn insert(&mut self, row: Self::Data);
    fn delete(&mut self, predicate: impl Fn(&Self::Data) -> bool);
    fn select(&self, predicate: impl Fn(&Self::Data) -> bool) -> Vec<&Self::Data>;

    fn insert_many(&mut self, rows: Vec<Self::Data>) {
        for row in rows {
            self.insert(row);
        }
    }
}

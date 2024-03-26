use std::sync::OnceLock;

use table::{Column, DataType, Table, TableMD};

pub mod builtin;
pub mod database;
pub mod index;
pub mod table;

pub static TABLE: OnceLock<TableMD> = OnceLock::new();

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
                        name: "addr".to_owned(),
                        data_type: DataType::String,
                    },
                ],
            },
        ]);

        table
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

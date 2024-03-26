use crate::{Metadata, TABLE};

#[derive(Debug, PartialEq, Clone)]
pub struct TableMD {
    pub name: String,
    pub tables: Vec<Table>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table {
    pub name: String,
    pub columns: Vec<Column>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Column {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Int,
    String,
    Boolean,
    Timestamp,
}

impl TableMD {
    pub fn new(name: &str) -> Self {
        TableMD {
            name: name.to_owned(),
            tables: vec![],
        }
    }
}

impl Metadata for TableMD {
    type Data = Table;

    fn insert(&mut self, row: Self::Data) {
        self.tables.push(row);
    }

    fn delete(&mut self, predicate: impl Fn(&Self::Data) -> bool) {
        self.tables.retain(|row| !predicate(row));
    }

    fn select(&self, predicate: impl Fn(&Self::Data) -> bool) -> Vec<&Self::Data> {
        self.tables.iter().filter(|row| predicate(row)).collect()
    }
}

pub fn table_exists(table_md: &TableMD, table_name: &str) -> bool {
    !table_md.select(|tb| tb.name == table_name).is_empty()
}

pub fn columns_exists(table_md: &TableMD, table_name: &str, column_name: &str) -> bool {
    let tables = table_md.select(|tb| tb.name == table_name);

    if tables.is_empty() {
        return false;
    }

    tables[0].columns.iter().any(|col| col.name == column_name)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_db_table_create() {
        let mut table = TableMD::new("users");

        table.insert_many(vec![Table {
            name: "name".to_owned(),
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
        }]);

        assert_eq!(table.tables.len(), 1);

        assert_eq!(
            table.select(|tb| tb.name == "name"),
            vec![&Table {
                name: "name".to_owned(),
                columns: vec![
                    Column {
                        name: "id".to_owned(),
                        data_type: DataType::Int,
                    },
                    Column {
                        name: "name".to_owned(),
                        data_type: DataType::String,
                    },
                ]
            },]
        )
    }

    #[test]
    fn test_utils_fn() {
        let mut table = TableMD::new("table metadata");

        table.insert_many(vec![Table {
            name: "users".to_owned(),
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
        }]);

        assert!(table_exists(&table, "users"));
        assert!(!table_exists(&table, "not_exist"));

        assert!(columns_exists(&table, "users", "id"));
        assert!(!columns_exists(&table, "users", "not_exist"));
        assert!(!columns_exists(&table, "not_exits", "not_exist"));
    }
}

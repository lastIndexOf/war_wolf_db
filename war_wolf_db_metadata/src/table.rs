use crate::Metadata;

#[derive(Debug, PartialEq, Clone)]
pub struct TableMD {
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
            columns: vec![],
        }
    }
}

impl Metadata for TableMD {
    type Data = Column;

    fn insert(&mut self, row: Self::Data) {
        self.columns.push(row);
    }

    fn delete(&mut self, predicate: impl Fn(&Self::Data) -> bool) {
        self.columns.retain(|row| !predicate(row));
    }

    fn select(&self, predicate: impl Fn(&Self::Data) -> bool) -> Vec<&Self::Data> {
        self.columns.iter().filter(|row| predicate(row)).collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_db_table_create() {
        let mut table = TableMD::new("users");

        table.insert_many(vec![
            Column {
                name: "id".to_owned(),
                data_type: DataType::Int,
            },
            Column {
                name: "name".to_owned(),
                data_type: DataType::String,
            },
        ]);

        assert_eq!(table.columns.len(), 2);

        assert_eq!(
            table.select(|col| col.name == "name"),
            vec![&Column {
                name: "name".to_owned(),
                data_type: DataType::String,
            },]
        )
    }
}

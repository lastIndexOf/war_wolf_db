pub mod builtin;
pub mod database;
pub mod index;
pub mod table;

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

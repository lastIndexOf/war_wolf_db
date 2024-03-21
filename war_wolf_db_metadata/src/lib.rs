pub mod database;
pub mod index;
pub mod table;

pub trait Metadata {
    type Row;

    fn create() -> Self;
    fn insert(&mut self, row: Self::Row);
    fn delete(&mut self, row: &Self::Row);
}

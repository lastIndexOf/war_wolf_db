use crate::Metadata;

pub struct FuncMD {
    name: String,
    funcs: Vec<Func>,
}

pub struct Func {
    pub func_name: String,
    pub args_num: usize,
    pub callback: Box<dyn Fn(Vec<String>) -> u32 + Send + Sync>,
    pub is_aggregate: bool,
}

impl FuncMD {
    pub fn new(name: &str) -> Self {
        FuncMD {
            name: name.to_owned(),
            funcs: vec![],
        }
    }
}

impl Metadata for FuncMD {
    type Data = Func;

    fn insert(&mut self, row: Self::Data) {
        self.funcs.push(row);
    }

    fn delete(&mut self, predicate: impl Fn(&Self::Data) -> bool) {
        self.funcs.retain(|row| !predicate(row));
    }

    fn select(&self, predicate: impl Fn(&Self::Data) -> bool) -> Vec<&Self::Data> {
        self.funcs.iter().filter(|row| predicate(row)).collect()
    }
}

pub fn func_exists(func_md: &FuncMD, func_name: &str) -> bool {
    !func_md
        .select(|func| func.func_name == func_name)
        .is_empty()
}

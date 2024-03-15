pub mod error;
pub mod macros;

pub type Error = crate::error::miette::Error;
pub type Result<R, E = Error> = std::result::Result<R, E>;

pub mod prelude {
    pub use crate::{Error, Result};
}

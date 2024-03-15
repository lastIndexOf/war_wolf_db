pub use miette;

#[cfg(test)]
mod test_error {
    use crate::Result;

    #[test]
    fn test_custom_error() -> Result<()> {
        Ok(())
    }
}

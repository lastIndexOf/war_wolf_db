#[macro_export]
macro_rules! error {
  (@error $expr:expr) => {
    $crate::error::miette::miette!($expr)
  };
  ($str:literal $(,)?) => {{
    let err = format!($str);
    $crate::error!(@error err)
  }};
  ($expr:expr $(,)?) => {{
    $crate::error!(@error $expr)
  }};
  ($fmt:expr, $($arg:tt)*) => {{
    let err = format!($fmt, $($arg)*);
    $crate::error!(@error err)
  }};
}

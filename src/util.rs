macro_rules! error {
    ($span:expr, $msg:literal $(,)?) => {
        ::syn::Error::new($span, $msg)
    };
    ($span:expr, $msg:literal $(,$args:expr)+ $(,)?) => {
        ::syn::Error::new($span, format!($msg $(,$args)*))
    };
}

macro_rules! fail {
    ($($args:tt)*) => {
        return Err(error!($($args)*));
    };
}

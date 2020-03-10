#[macro_export(local_inner_macros)]

macro_rules! err_fatal {
    (src: $src:expr, span: $span:expr, title: $tgt:expr, msg: $($msg:tt)+) => (
        {
            $crate::report($tgt, &__l_args!($($msg)+));
            $crate::spit_line($span, $src);
        }
    )
}

#[macro_export]
macro_rules! __l_args {
    ($($args:tt)*) => {
        format!("{}", format_args!($($args)*))
    }
}
/*
 * rt_error! {
 *     src: file,
 *     span: span,
 *     message: format!("Hello world", a, b, c,)
 * }
 *
 * */

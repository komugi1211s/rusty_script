#[macro_export(local_inner_macros)]
macro_rules! err_fatal {
    (src: $src:expr, span: $span:expr, title: $tgt:expr, msg: $($msg:tt)+) => (
        {
            $crate::report($tgt, &__l_args!($($msg)+));
            $crate::spit_line($src, $span);
        }
    )
}

#[macro_export(local_inner_macros)]
macro_rules! expect {
    ($x: expr, $msg: expr) => (
        {
            match $x 
            {
                Ok(x) => x,
                Err(_) => 
                {
                    $crate::report("internal", $msg);
                    ::std::panic!($msg)
                }
            }
        }
    )
}

#[macro_export]
macro_rules! __l_args {
    ($($args:tt)*) => {
        format!("{}", format_args!($($args)*))
    }
}

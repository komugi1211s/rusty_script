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
    ($x: expr, $($msg:tt)+) => (
        {
            let ___file = ::std::file!();
            let ___line = ::std::line!();
            let ___stringified = ::std::stringify!($x);
            match $x 
            {
                Ok(x) => x,
                Err(_) => 
                {
                    $crate::report_compiler_bug(
                        &__l_args!($($msg)+),
                        ___file,
                        ___line,
                        ___stringified
                    );
                    if ::std::cfg!(debug_assertions) {
                        ::std::panic!();
                    } else {
                        ::std::process::exit(1);
                    }
                }
            }
        }
    )
}

#[macro_export(local_inner_macros)]
macro_rules! expect_opt {
    ($x: expr, $($msg:tt)+) => (
        {
            let ___file = ::std::file!();
            let ___line = ::std::line!();
            let ___stringified = ::std::stringify!($x);
            match $x 
            {
                Some(x) => x,
                None => 
                {
                    $crate::report_compiler_bug(
                        &__l_args!($($msg)+),
                        ___file,
                        ___line,
                        ___stringified
                    );
                    if ::std::cfg!(debug_assertions) {
                        ::std::panic!();
                    } else {
                        ::std::process::exit(1);
                    }
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

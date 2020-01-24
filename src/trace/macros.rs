


#[macro_export(local_inner_macros)]
macro_rules! err_fatal {
    (src: $src:expr, span: $span:expr, title: $tgt:expr, msg: $($msg:tt)+) => (
        {
            use $crate::log::Log;
            let mut __x = $crate::log::RecordBuilder::new();
            if !$span.is_invalid() {
                __x.line(Some($span.start));
            }
                

            $crate::Logger.log(
                &__x.level($crate::log::Level::Error)
                .target($tgt)
                .args(__l_args!($($msg)+))
                .file(Some(&$src.filename))
                .build()
            );
        }
    )
}

#[macro_export]
macro_rules! code_line {
    (src: $src:expr, span: $span:expr) => {
        code_line!(src: $src, span: $span, pad: 0);
    };
    (src: $src:expr, span: $span:expr, pad: $pad:expr) => {
        {
            $crate::Logger.spit_line($span, &$src, $pad);
        }
    };
}


#[macro_export]
macro_rules! err_rt {
    (src: $src:expr, span: $span:expr, msg: $msg:expr) => {
        {
            let mut __x = RecordBuilder::new()
                .level(log::Level::Error)
                .target("RUNTIME ERROR")
                .args(__l_args!($msg))
                .file(Some(&$src.filename));
                
                if !$span.is_invalid() {
                    __x.line(Some($span.start));
                }

                let __y = __x.build();

            Logger.log(&__y);
        }
    }
}

#[macro_export]
macro_rules! err_internal {
    ($msg:expr) => {
        error!(target: "internal", "{}", $msg);
    }
}

#[macro_export]
macro_rules! __l_args {
    ($($args:tt)*) => {
        format_args!($($args)*)
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

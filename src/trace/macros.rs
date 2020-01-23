
#[macro_export(local_inner_macros)]
macro_rules! err_bc {
    (src: $src:expr, span: $span:expr, msg: $msg:expr) => (
        {
            use $crate::log::Log;
            let mut __x = $crate::log::RecordBuilder::new();
                __x
                .level($crate::log::Level::Error)
                .target("BYTECODE GENERATOR ERROR")
                .args(__l_args!($msg))
                .file(Some(&$src.filename));
                
            if !$span.is_invalid() {
                __x.line(Some($span.start));
            }

            let __y = __x.build();

            $crate::Logger.log(&__y);
        }
    )
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
                    __x.line(Some($span.start))
                    .key_value(&$src.kv_from_span(span));
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
    };
}
/*
 * rt_error! {
 *     src: file,
 *     span: span,
 *     message: format!("Hello world", a, b, c,)
 * }
 *
 * */


use super::{ Logger };
use super::log::{self, Record, RecordBuilder };

macro_rules! rt_error {
    (src: $src:expr, span: $spn:expr, msg: $msg:expr) => {
        {
            let mut __x = RecordBuilder::new()
                .level(log::Level::Error)
                .target("RUNTIME ERROR")
                .args(format_args!($msg))
                .file(Some(&$src.filename));
                
                if !$span.is_invalid() {
                    __x.line(Some($span.start))
                    .key_value($src.kv_from_span(span));
                }

                let __y = __x.build();

            Logger.log(&__y);
        }
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

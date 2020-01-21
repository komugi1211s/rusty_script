
use super::{ Error };

macro_rules! rt_error {
    ($rep:expr, $spn:expr, $msg:expr) => {
        {
            let x = Error::new_on_runtime(String::from($msg), $spn);
            $rep.report_error(x);
        }
    };
    ($rep:expr, $spn:expr, $msg:expr, $($args:expr),*) => { 
        {
            let msg = format!($msg, $($args,)*);
            rt_error!($rep, $spn, msg)
        }
    };
}

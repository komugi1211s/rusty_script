
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn infer(a: i32, b: i32) {
    return Some(a + b);
}

fn main() {
    println!("Hello!");
    let x = add(1, 2);
    let y = infer(1, 2);
    println!("{}", x);
}

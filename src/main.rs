mod parse;

fn main() {
    match parse::parse_literal("type asdf = string") { 
        Ok(_) => println!("SUCC"),
        Err(err) => { dbg!(err); }
    }
}

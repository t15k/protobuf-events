use std::io::Read;
use std::fs::File;

extern crate base64;
mod protobuf_events;
mod query_parser;
mod query_engine;
// TODO Support packed repeated fields

fn main() {

    query_parser::Tokenizer::new(".hello.name");

    let mut desc_filename = String::from("");
    let mut expression: Option<String> = None;
    let mut it = std::env::args().skip(1);
    loop {
        match it.next() {
            Some(s) => match s.as_ref() {
                "-d" => match it.next() {
                    Some(v) => desc_filename = v,
                    None => {
                        eprintln!("args for --desc or -d is required!");
                        std::process::exit(1);
                    }
                },
                "-e" => match it.next() {
                    None => {
                        eprintln!("expression must follow -e");
                        std::process::exit(2);
                    },
                    v => expression = v
                },
                _ => {
                    eprintln!("unknown option {}", s) ;
                    std::process::exit(3);
                }
            },
            None => break
        }
    }
    
    let type_map = match File::open(desc_filename) {
        Ok(mut desc_file) => protobuf_events::parse_file_descriptor_proto_set(&mut desc_file),
        Err(e) => {
            eprintln!("Could not read descriptor file. Os says {}.", e);
            std::process::exit(1);
        }
    };
	// TODO something should tell the root message type!
    protobuf_events::parse_any_message(&mut std::io::stdin(), type_map.get("Test").unwrap());
    let engine = query_engine::Engine::from_text(&String::from("some.where"));
    for proto_token in protobuf_events::Tokenizer::new(&mut std::io::stdin()) {
        engine.accept(proto_token)
        // pass result to result output
    }
}

struct Iter<'a>(&'a mut dyn Read);
//struct Item(i64, u64, i64);
impl<'a> Iterator for Iter<'a>{
	type Item = Result<(i64, u64, i64), ()>;
	fn next(&mut self) -> Option<Result<(i64, u64, i64), ()>> {
		let mut re = &mut self.0;
		match protobuf_events::read_varu64(&mut re) {
			Ok(i) => Some(Ok((1, i, 3))),
			Err(_) => None
		}
	}
}

mod test{
    #[test]
    fn test_do_like_this() {
        let mut reg :std::collections::HashMap<i64, i64> = std::collections::HashMap::new();
        reg.insert(1, 2);
        let r = [1u8, 2];
        let mut re = &r[..];
        let mut count = 0;
        for res in super::Iter(&mut re) {
            match res {
                Ok((k, v, t)) => {
                    count += 1;
                    match reg.get(&k) {
                        Some(h) => {println!("HELLO");super::protobuf_events::read_varu64(&mut &r[..]);},//handle(v),
                        None => continue
                    }
                }
                Err(_) => ()
            }
        }
        assert_eq!(2, count);
    }
}


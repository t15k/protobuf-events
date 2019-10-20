use std::io::Read;
use std::fs::File;
use std::collections::HashMap;

extern crate base64;
mod query_parser;
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
        Ok(mut desc_file) => parse_file_descriptor_proto_set(&mut desc_file),
        Err(e) => {
            eprintln!("Could not read descriptor file. Os says {}.", e);
            std::process::exit(1);
        }
    };
	// TODO something should tell the root message type!
    parse_any_message(&mut std::io::stdin(), type_map.get("Test").unwrap());
    //println!("{:?}", &type_map);
	//println!("I made it");
}
#[test]
fn test_do_like_this() {
	let mut reg :std::collections::HashMap<i64, i64> = std::collections::HashMap::new();
	reg.insert(1, 2);
	let r = [1u8, 2];
	let mut re = &r[..];
	let mut count = 0;
	for res in Iter(&mut re) {
		match res {
			Ok((k, v, t)) => {
				println!("AAAAAA");
				count += 1;
				match reg.get(&k) {
					Some(h) => {println!("HELLO");read_varu64(&mut &r[..]);},//handle(v),
					None => continue
				}
			}
			Err(_) => ()
		}
	}
	assert_eq!(2, count);
}
struct Iter<'a>(&'a mut Read);
//struct Item(i64, u64, i64);
impl<'a> Iterator for Iter<'a>{
	type Item = Result<(i64, u64, i64), ()>;
	fn next(&mut self) -> Option<Result<(i64, u64, i64), ()>> {
		let mut re = &mut self.0;
		match read_varu64(&mut re) {
			Ok(i) => Some(Ok((1, i, 3))),
			Err(_) => None
		}
	}
}

// TODO Support packed repeated fields

#[derive(Debug, Clone)]
enum WireType {
    VarInt(u32),
    Bit64(u32),
    LengthDelimited(u32),
    StartGroup,
    EndGroup,
    Bit32(u32),
    Unknown(u8, u32)
}

#[derive(Debug, Clone)]
enum WireField {
    VarInt(u32, u64),
    Bit64(u32, u64),
    LengthDelimited(u32, Vec<u8>),
    Bit32(u32, u32),
    StartGroup,
    EndGroup,
    Unknown(u8, u32)
}

struct SchemaField {
    name :String,
    field_number: u32,
    repeated :bool,
    kind :SchemaType2
}

#[derive(Debug)]
enum SchemaType2 {
    Double,
    Float,
    Int32,
    Int64,
    UInt32,
    UInt64,
    SInt32,
    SInt64,
    Fixed32,
    Fixed64,
    SFixed32,
    SFixed64,
    Bool,
    String,
    Bytes,
    Message,
    Enum,
    Group,
    Unknown
}

fn schema_type2(n :u64) -> SchemaType2 {
    return match n {
        1 => SchemaType2::Double,
        2 => SchemaType2::Float,
        3 => SchemaType2::Int64,
        4 => SchemaType2::UInt64,
        5 => SchemaType2::Int32,
        6 => SchemaType2::Fixed64,
        7 => SchemaType2::Fixed32,
        8 => SchemaType2::Bool,
        9 => SchemaType2::String,
        10 => SchemaType2::Group,
        11 => SchemaType2::Message,
        12 => SchemaType2::Bytes,
        13 => SchemaType2::UInt32,
        14 => SchemaType2::Enum,
        15 => SchemaType2::SFixed32,
        16 => SchemaType2::SFixed64,
        17 => SchemaType2::SInt32,
        18 => SchemaType2::SInt64,
        _ => SchemaType2::String
    }
}


#[derive(Debug, Clone)]
struct Meta {
    name :String,
    field_number: u32,
    repeated :bool
}

struct ProtoBuffer <'a> {
    read: &'a mut dyn Read,
}

impl <'a> Iterator for ProtoBuffer <'a>{
    type Item = WireField;
    fn next(&mut self) -> Option<Self::Item> {
        return match read_key(self.read) {
            Ok(wt) => match wt {
                WireType::VarInt(id) => match read_varu64(self.read) {
                    Ok(v) => Some(WireField::VarInt(id,v)),
                    Err(e) => panic!(e)
                },
                WireType::LengthDelimited(id) => match read_bytes(self.read) {
                    Ok(v) => Some(WireField::LengthDelimited(id, v)),
                    Err(e) => panic!(e)
                },
                WireType::Bit32(id) => 
                    Some(WireField::Bit32(id, read_fixedu32(self.read))),
                WireType::Bit64(id) =>
                    Some(WireField::Bit64(id, read_fixedu64(self.read))),
                WireType::StartGroup =>
                    Some(WireField::StartGroup),
                WireType::EndGroup =>
                    Some(WireField::EndGroup),
                WireType::Unknown(id, v) =>
                    Some(WireField::Unknown(id, v))
            },
            Err(ReadStatus::Empty) => None,
            Err(e) => {
                println!("ERROR: {:?}", e);
                panic!("")}
        };
    }
}

fn raw_parse(r :&mut dyn Read) -> ProtoBuffer {
    return ProtoBuffer { 
        read: r 
    }; 
}

fn parse_file_descriptor_proto_set(r :&mut dyn Read) -> HashMap<String, HashMap<u32, SchemaField>> {
    let mut names_to_types = HashMap::new();
    for e in raw_parse(r) {
        match e {
            WireField::LengthDelimited(1, v) => {
                parse_file_descriptor_proto(&mut &v[..], &mut names_to_types);
            },
            _ => ()
        }
    }
    return names_to_types;
}

fn parse_file_descriptor_proto(r :&mut dyn Read, 
                               type_names :&mut HashMap<String, HashMap<u32, SchemaField>>) {
    let mut package_name = String::from(""); // TODO we can be more clever
    for e in raw_parse(r) {
        match e {
            WireField::LengthDelimited(1, v) => {
                match String::from_utf8(v) {
                    Ok(_) => (),//println!("OK: {:?}", s),
                    Err(e)=> eprintln!("ERR: {:?}", e)
                }
            },
            WireField::LengthDelimited(2, v) => {
                match String::from_utf8(v) {
                    Ok(s) => package_name = s,
                    Err(e) => println!("ERR: {:?}", e)
                }
            },
            WireField::LengthDelimited(4, v) => {
                let (name, map) = parse_descriptor_proto(&mut &v[..]);
                type_names.insert(name, map);
                // TODO use return value
            },
            _ => ()
        }
    }
}

/// Parse a message descriptor.
fn parse_descriptor_proto<'a>(r :&mut dyn Read) -> (String, HashMap<u32, SchemaField>) {
    let mut message_name = String::from(""); 
    let mut field_map = HashMap::new();
    for e in raw_parse(r) {
        match e {
            WireField::LengthDelimited(1, v) => match String::from_utf8(v) {
                Ok(s) => message_name = s,
                Err(e) => println!("{:?}", e) // TODO
            },
            WireField::LengthDelimited(2, v) => {
                let schema_field = parse_field_descriptor_proto(&mut &v[..]);
                field_map.insert(schema_field.field_number, schema_field);
            }
            _ => ()
        }
    }
    return (message_name, field_map);;
}

fn parse_field_descriptor_proto(r :&mut dyn Read) -> SchemaField {
    let mut meta = SchemaField{
        field_number: 0,
        name: String::from(""),
        repeated: false,
        kind: SchemaType2::Unknown
    };
    for e in raw_parse(r) {
        match e {
            WireField::LengthDelimited(1, v) => match String::from_utf8(v) {
                Ok(s) => meta.name = s,
                Err(e) => println!("{:?}", e) // TODO, Error handling.
            },
            WireField::VarInt(3, n) => meta.field_number = n as u32,
            WireField::VarInt(4, n) => meta.repeated = n == 3, // 3 is enum REPATED
            WireField::VarInt(5, n) => meta.kind = schema_type2(n),
            _ => ()
        }
    }
    return meta;
}

/// Read the next field tag (not it's value) from r.
/// Advances r to the value following the next tag.
/// Answers a tuple (field_id, field_type)
fn read_key(r: &mut dyn Read) -> Result<(WireType), ReadStatus> {
	return match read_varu64(r) {
		Ok(tag) => {
			Ok(wire_type((tag & 0x07) as u8, (tag >> 3) as u32))
		},
		Err(e) => Err(e) // FIXME
	}
}

fn wire_type(wire_type :u8, id :u32) -> WireType {
    return match wire_type {
        0x00 => WireType::VarInt(id),
        0x01 => WireType::Bit64(id),
        0x02 => WireType::LengthDelimited(id),
        0x03 => WireType::StartGroup,
        0x04 => WireType::EndGroup,
        0x05 => WireType::Bit32(id),
        _ => WireType::Unknown(wire_type, id)
    }
}


// Handler takes a reader and will unmarshal to type
// handled by the handler, it will consume bytes from the
// as a side effect of this.
trait Handle {
	fn handle(&self, r: &mut dyn std::io::Read);
}
trait Sink<T> {
	fn accept(&mut self, v: T);
}

#[derive(Debug)]
enum ReadStatus {
	Overflow,
	Empty,
	IO(std::io::ErrorKind)
}

// read_varu64 reads an unsigned varint from the reader r.
fn read_varu64(r: &mut dyn Read) -> Result<u64, ReadStatus> {// (uint64, error) {
	let mut x: u64 = 0;
	let mut s: u32 = 0;
	let i: u32 = 0;
	let mut buf = [0; 1];
	loop {
		let size = match r.read(&mut buf) {
			Ok(s) => s,
			Err(r) => return Err(ReadStatus::IO(r.kind()))
		};
		if size > 0 {
			let v = buf[0];
			if v < 0x80 {
				if i > 9 || i == 9 && v > 1 {
					return Err(ReadStatus::Overflow)
				}
				return Ok(x | u64::from(buf[0])<<s)
			}
			x |= u64::from(v & 0x7f) << s;
			s += 7;
		} else {
			return Err(ReadStatus::Empty)
		}
	}
}

#[test]
fn test_read_varu64(){
	let simple = [0x01];
	let boundary = [0x81, 0x01];
	let boundary_one = [0x80, 0x01];
	let max = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01];
	// More tests
	// 1) for error where we run out of byte, but 8th bit was set.
	// 2) Overflow, in 11th byte
	// 3) Overflow, 12 or more bytes.
	let tcs = [(1, &simple[..]), (129, &boundary[..]), (128, &boundary_one[..]),
			   (std::u64::MAX, &max)];
	for tc in tcs.iter() {
		let mut v = tc.1;
		match read_varu64(&mut v) {
			Ok(v) => assert_eq!(v, tc.0),
			Err(_) => assert_eq!(1, 2)
		}
	}
}

/// Must complete with an error
#[test]
fn test_read_varu64_to_few_byte(){
	let mut one_short = &[0x81, 0x81][..];
	match read_varu64(&mut one_short) {
		Ok(_) => assert_eq!(1, 2),
		Err(v) => match v { // we shouldn't end up here
			ReadStatus::Empty => (),
			_ => assert!(false)
		}
	}
}
fn read_bytes<'a>(r :&'a mut dyn Read) -> Result<Vec<u8>, std::io::Error> {
	match read_varu64(r) {
		Ok(l) => {
            let mut t = r.take(l);
            let mut v = Vec::with_capacity(l as usize);
            match t.read_to_end(&mut v) {
                Ok(_) => Ok(v),
                Err(e) => Err(e)
            }
        },
		Err(_) => Err(std::io::Error::new(std::io::ErrorKind::Other, "oh no"))
	}
}
fn read_fixedu32(r : &mut dyn Read) -> u32 {
    let mut buf = [0; 4];
    r.read_exact(&mut buf); // TOOO, were not checking the result
    return buf[0] as u32 
        | (buf[1] as u32) << 8 
        | (buf[2] as u32) << 16
        | (buf[3] as u32) << 24;
}
fn read_fixedu64(r : &mut dyn Read) -> u64 {
    return (read_fixedu32(r) as u64)| (read_fixedu32(r) as u64) << 32;
}

fn decode_zigzag32(n :u64) -> i32 {
    return ((n >> 1) ^ (-((n & 1) as i64)) as u64) as i32
}
#[test]
fn zigzag32_decoding() {
    assert_eq!(-1, decode_zigzag32(1));
    assert_eq!(1, decode_zigzag32(2));
    assert_eq!(-2, decode_zigzag32(3));
    assert_eq!(2147483647, decode_zigzag32(4294967294));
    assert_eq!(-2147483648, decode_zigzag32(4294967295));
}

fn decode_zigzag64(n :u64) -> i64{
    return ((n >> 1) ^ (-((n & 1) as i64)) as u64) as i64
}
#[test]
fn zigzag64_decoding() {
    assert_eq!(-1, decode_zigzag64(1));
    assert_eq!(1, decode_zigzag64(2));
    assert_eq!(-2, decode_zigzag64(3));
    assert_eq!(2147483648, decode_zigzag64(4294967296));
    assert_eq!(-2147483649, decode_zigzag64(4294967297));
}
fn decode_sfixed32(n :u32) -> i32 { return n as i32;}

fn decode_fixed32(n :u32) -> u32 { return n;}

fn decode_float(n :u32) -> f32 { return f32::from_bits(n); }
#[test]
fn float_decoding() {
    let a = [205, 204, 204, 63];
    let n = read_fixedu32(&mut &a[..]);
    let f = decode_float(n);
    assert_eq!(1.6, f);
}

fn decode_sfixed64(n :u64) -> i64 { return n as i64; }

fn decode_fixed64(n :u64) -> u64 { return n as u64; }

fn decode_double(n :u64) -> f64 { return f64::from_bits(n); }
#[test]
fn double_decoding() {
    let a = [51, 51, 51, 51, 51, 51, 3, 64];
    let n = read_fixedu64(&mut &a[..]);
    let f = decode_double(n);
    assert_eq!(2.4, f);


}

// ------------------------------
// High level readers
// ------------------------------

fn parse_any_message(r :&mut dyn Read, type_map :&HashMap<u32, SchemaField>) {
    let mut arrays :HashMap<u32, Vec<String>> = HashMap::new();
	print!("{{");
	let mut last_output = false;
    let mut c = || {
        if last_output {print!(",")}
        last_output = true;
    };
    {
	let mut f = |x, z, c :&mut dyn FnMut()| {
        out_json(x, z, &mut arrays, c);
    };
	for wf in raw_parse(r) {
		match wf {
			WireField::VarInt(fid, v) =>  match type_map.get(&fid) {
                Some(map_type_v) => match map_type_v.kind {
                    SchemaType2::Int32 =>  f(map_type_v, json_token(&(v as i32)), &mut c),
                    SchemaType2::Int64 => f(map_type_v, json_token(&v), &mut c),
                    SchemaType2::UInt32 => f(map_type_v, json_token(&(v as u32)), &mut c),
                    SchemaType2::UInt64 => f(map_type_v, json_token(&(v as u64)), &mut c),
                    SchemaType2::SInt32 => f(map_type_v, json_token(&decode_zigzag32(v)), &mut c),
                    SchemaType2::SInt64 => f(map_type_v, json_token(&decode_zigzag64(v)), &mut c),
                    SchemaType2::Bool => f(map_type_v, json_bool_token(v), &mut c),
                    SchemaType2::Enum => (),
                    _ => () // Unknown combi, ignore
                },
                None => () //ignore
			},
			WireField::Bit64(fid, v) => match type_map.get(&fid) {
                Some(schema_field) => match schema_field.kind {
                SchemaType2::SFixed64 => f(schema_field, json_token(&decode_sfixed64(v)), &mut c),
                SchemaType2::Fixed64 => f(schema_field, json_token(&decode_fixed64(v)), &mut c),
                SchemaType2::Double => f(schema_field, json_token(&decode_double(v)), &mut c),
                    _ => () // ignore
                },
                None => () // ignore
			},
			WireField::LengthDelimited(fid, v) => match type_map.get(&fid) {
                Some(schema_field) => match schema_field.kind {
                    SchemaType2::String => match String::from_utf8(v) {
				    	Ok(s) => f(schema_field, json_str_token(&s), &mut c),
					    Err(_) => () // ignore corrupt string
				    },
				    SchemaType2::Bytes =>
                        f(schema_field, json_str_token(&base64::encode(&v)), &mut c),
				    SchemaType2::Message => {
                        c();
                        parse_sub_message(&mut &v[..], &schema_field.name, type_map, false);
                    },
				    _ => () // Unknown combi, ignore
                },
			    None => () // unknown field, ignore
			},
			WireField::Bit32(fid, v) => match type_map.get(&fid) {
                Some(schema_field) => match schema_field.kind {
                    SchemaType2::SFixed32 => f(schema_field, json_token(&decode_sfixed32(v)), &mut c),
                    SchemaType2::Fixed32 => f(schema_field, json_token(&decode_fixed32(v)), &mut c),
                    SchemaType2::Float => f(schema_field, json_token(&decode_float(v)), &mut c),
                    _ => ()
                },
                None => () // ignore
			},
			WireField::StartGroup => (), // ignore
			WireField::EndGroup => (), // ignore
			WireField::Unknown(_, _) => ()//ignore
		}
	}
    }
    for (k, array_v) in arrays.iter() {
        let mut acomma = false;
        c();
        print!("\"{}\":[", &type_map.get(&k).unwrap().name);
        for a in array_v {
            if acomma { print!(","); }
            acomma = true;
            print!("{}", a);
        }
        print!("]");
    }
	print!("}}");
}

fn parse_sub_message(r :&mut dyn Read, name :&String, type_map :&HashMap<u32, SchemaField>, last_output: bool) {
    if last_output { print!(","); }
    print!("{}:", json_str_token(name));
    parse_any_message(r, type_map);
}

fn out_json(meta :&SchemaField, json_v :String, m :&mut HashMap<u32, Vec<String>>, c :&mut dyn FnMut()) {
    if meta.repeated {
        if m.contains_key(&meta.field_number) {
            m.get_mut(&meta.field_number).unwrap().push(json_v);
        } else {
            m.insert(meta.field_number, vec!(json_v));
        }
    } else {
        c();
        print!("\"{}\": {}", &meta.name, &json_v);
    }
}


fn json_str_token(v :&String) -> String {
    return format!("\"{}\"", v);
}
fn json_bool_token(v :u64) -> String {
    return match v {
        0 => "false".to_string(),
        _ => "true".to_string()
    }
}
fn json_token(v :& dyn std::fmt::Display) -> String  { return format!("{}", v); }

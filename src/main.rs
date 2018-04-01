use std::io::Read;

trait Go {
	fn go(&mut self, r: &std::io::Read);
}

struct Goer {

}
impl Go for Goer {
	fn go(&mut self, _: &std::io::Read){

	}
}

fn go_some() {
	let mut goer = Goer{};
	let mut handlers: std::collections::HashMap<Vec<u64>, &mut Go> =
		std::collections::HashMap::new();
	handlers.insert(vec![1], &mut goer);
	let mut h = handlers.get(&vec![1]).unwrap();
	(*h).go(&std::io::stdin())
}

fn main() {
	println!("Stream proto");
	// https://github.com/google/protobuf/blob/master/src/google/protobuf/descriptor.proto
	// root is a file descriptor set
	// 	 []FileDescriptorProto file = 1;
	//

	// parse template and create a handler setup
	// TODO: JSON templates, raw transformation, schema definitions support
	let mut x = 1;
	let one_handler = |_: & Read| {x += 1; ()};
	let two_handler = |_: & Read| ();
	//two_handler(&mut std::io::stdin());
	let mut handlers: std::collections::HashMap<Vec<u64>, &FnMut(&Read) -> ()> =
		std::collections::HashMap::new();
//	let mut handlers: std::collections::HashMap<Vec<u64>, &FnMut(&Read) -> ()> =
//		std::collections::HashMap::new();

	handlers.insert(vec![1], &one_handler);
	//handlers.insert(vec![1, 1], Box::new(two_handler));
	//let h = handlers.get_mut(&vec![1]).unwrap();//&mut &FnMut(&Read)
	//(*h)(&std::io::stdin());
	parse(&mut std::io::stdin(), &mut |fid, t, r| {
		println!("fid = {}, t = {}", fid.get(0).unwrap(), t);
		match t {
			0x00 => match read_varu64(r) { // varints signed and unsigned
				Ok(v) => {
					println!("{}: {}", fid.get(0).unwrap(), v);
				}
				Err(_) => panic!("Error")
				//Some(h) => h.handle(r),//; err != nil {
				//None => return
			},
			0x01 => panic!("fixed64, sfixed64, double not supported"),
			0x02 => { // Length delimeted, string, messages and bytes.
				match read_varu64(r) {
					Ok(size) =>	{
						println!("will read {} bytes", size);
						let mut tak = r.take(size);
						match handlers.get_mut(fid) {
							Some(sub_handle) => {
								// TODO recurse it a message, or pass to message handler
								// let mut s = String::new();
								let ss = *sub_handle;
								ss(&mut tak);
								let mut s = vec![];
								println!("take limit{}", tak.limit());
								match tak.read_to_end(&mut s) {
									Ok(_) => (),//parse_(idf, ), //,panic!(""),
									Err(e) => {
										panic!(format!("{}", e));
									}
								}
							},
							None => {
								let mut buf = [0; 1];
								for _ in (1..size).rev() {
									tak.read(&mut buf);
								}
							}
						}
					},
					Err(_) => ()
				}
				// would have called something on e
				// read bytes a skip
				//panic!("string, bytes, embedded messages, packed repeated fields not supported");
			},
			0x03 => panic!("start groups (deprecated) not supported"),
			0x04 => panic!("end groups (deprecated) not supported"),
			0x05 => panic!("fixed32, sfixed32, float not supported"),
			_ => panic!("not supported")
		}
		println!("field: {},  type: {}.", fid.get(0).unwrap(), t);
	});
	//a.parse_bytes(&mut &b[..]);
	println!("I made it");
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


fn next_field(r: &mut Read) -> Result<(u64, u8), ()> {
	return match read_varu64(r) {
		Ok(tag) => {
			let field_type :u8 = (tag & 0x07) as u8;
			let field_id = tag >> 3;
			Ok((field_id, field_type))
		},
		Err(_) => Err(()) // FIXME
	}
}

// --------------------------------------
#[test]
fn test_or_do_like_this() {
	let mut reg :std::collections::HashMap<i64, i64> = std::collections::HashMap::new();
	reg.insert(1, 2);
	let r = [1u8, 2];
	let mut count = 0;
	parse(&mut &r[..], &mut |fid, t, re| {
		let mut rr = [0; 1];
		re.read(&mut rr);
		count += 1});
	assert_eq!(1, count);
}
#[test]
fn test_or_another_do_like_this() {
	let mut reg :std::collections::HashMap<i64, i64> = std::collections::HashMap::new();
	reg.insert(1, 2);
	let r = [1u8, 2];
	parse(&mut &r[..], &mut |_, _, _| {});
	//assert_eq!(1, count);
}

fn parse<F>(r :&mut Read, f :&mut F) where F :FnMut(&Vec<u64>, u8, &mut Read) {
	let mut idp = vec![];
	parse_(r, f, &mut idp);
}

/// Reads next field from r (Read) and call f with the found field ID and field field_type.
/// The idp (Vector<i64>) has the current fields pushed and popped on return.
fn parse_<F>(r :&mut Read, f :&mut F, idp :&mut Vec<u64>) where F :FnMut(&Vec<u64>, u8, &mut Read) {
	loop {
		match next_field(r) {
			Ok((fid, t)) => {
				idp.push(fid);
				println!("000 {}, {}", idp.len(), t);
				f(idp, t, r);
				idp.pop();},
			Err(_) => return
		}
	}
}



// Handler takes a reader and will unmarshal to type
// handled by the handler, it will consume bytes from the
// as a side effect of this.
trait Handle {
	fn handle(&self, r: &mut std::io::Read);
}
trait Sink<T> {
	fn accept(&mut self, v: T);
}

struct MessageHandler<'a> {
	//field_handlers: std::collections::HashMap<u64, Box<Handle>>// map[uint64]Handler
	field_handlers: std::collections::HashMap<u64, &'a Handle>// map[uint64]Handler
	// u32 -> Handler taking
}
impl<'a> MessageHandler<'a> {
	fn new() -> Self {
		MessageHandler{field_handlers: std::collections::HashMap::new()}
	}
	// ParseBytes will parse the bytes with the current handlers
	// registered on this MessageHandler.
	fn parse_bytes(&mut self, b: &mut&[u8]) {
	    let v = [1u8, 2, 3];
		self.handle(b); // return
		self.handle(&mut &v[..]); // return
	}
	fn field(&mut self, fi: u64, h: &'a Handle) {
		self.field_handlers.insert(fi, h);
	}
}

impl<'a> Handle for MessageHandler<'a> {
	fn handle(&self, r: &mut Read) { //error {
		match read_varu64(r) { //binary.ReadUvarint(r)// error
			Ok(field) => {
				let field_type = field & 0x07;
				let field_id = field >> 3;
				match field_type {
					0x00 => match self.field_handlers.get(&field_id) {
						Some(h) => h.handle(r),//; err != nil {
						None => return
					}
					0x01 => panic!("fixed64, sfixed64, double not supported"),
					0x02 => panic!("string, bytes, embedded messages, packed repeated fields not supported"),
					0x03 => panic!("start groups (deprecated) not supported"),
					0x04 => panic!("end groups (deprecated) not supported"),
					0x05 => panic!("fixed32, sfixed32, float not supported"),
					_ => panic!("not supported")
				}
			},
			Err(_) => return // FIXME
		}
	}
}
#[test]
fn test_message_handler_handle() {
	let sq = [1u64, 2, 3];
	let mut v = vec![];
	let mut mh1 = MessageHandler::new();
	for s in sq.iter() {
		struct AA (u64);
		impl Sink<u64> for AA {
			fn accept(&mut self, i :u64) {
				self.0 = i;
			}
		}
		let aa: AA = AA(2);
		//v.push(|y: u64| y + 1);
		let x1 = std::cell::Cell::new(0);
		v.push(x1);
		let u64handler = Uint64Handler{c: Box::new(aa)};
///		//mh1.field(*s, Box::new(u64handler));
//		mh1.field(*s, &u64handler);
		let bytes = [8, 5];
		mh1.handle(&mut &bytes[..]);
	//	assert_eq!(5, x1.get())
	}
}


struct Int32Handler<'a> {
	callback: &'a Fn(i32)
}

impl<'a> Handle for Int32Handler<'a> {
	fn handle(&self, r: &mut Read) {
		match read_varu64(r) {
			Ok(v) => {
				if v <= std::u32::MAX as u64 {
					(self.callback)(v as i32)
				} else {
					println!("errropr")// FIXME ignoring error
				}
			},
			Err(r) => {
				println!("error {:?}", r);
			}
		}
	}
}
#[test]
fn test_int32_handler_handle() {
	let t255 = [255, 1];
	let tm5 = [0b1111_1011, 0xFF, 0xFF, 0xFF, 0b0000_1111];
	//let tm5 = [0x80, 0x80, 0x80, 0x80, 0b001_000];
	let tcs = [(255i32, &t255[..]), (-5, &tm5[..])];
	for tc in tcs.iter() {
		let x1 = std::cell::Cell::new(0i32);
		let h = Int32Handler{callback: &|y: i32| x1.set(y)};
		//let bytes = [255, 1];//, 255, 255, 255, 255, 255, 255];
		let mut bytes = tc.1;
		h.handle(&mut bytes);
		assert_eq!(tc.0, x1.get())
	}
}

struct StringHandler {}

struct Uint64Handler{c: Box<Sink<u64>>}


type MyNum = i64;
trait Inc {
	fn inc(&self) -> MyNum;
}
impl Inc for MyNum {
	fn inc(&self) -> MyNum {
		return self + 1;
	}
}

impl Handle for Uint64Handler {
	fn handle(&self, r: &mut Read) {// (err error) {
		match read_varu64(r) {
			Ok(v) => return,//self.c.accept(v),
			Err(_) => return // FIXME
		};
	}
}
#[test]
fn test_some() {
	let v1 = vec![1, 3];
	let v2 = vec![1, 3];

	assert_eq!(v1, v2);
	let mut v3 = vec![1];
	v3.push(3);
	assert_eq!(v1, v3);
	let mn: MyNum = 5;
	assert_eq!(7, mn.inc().inc());


}
//struct Int64Handler {
//	callback: Fn(i64)
//}

struct Uint32Handler<'a> {
	callback: &'a Fn(u32)
}
impl<'a> Handle for Uint32Handler<'a> {
	fn handle(&self, r: &mut Read) {
		match read_varu64(r) {
			Ok(v) => {
				if v <= std::u32::MAX as u64 {
					(self.callback)(v as u32)
				} // FIXME ignoring error
			},
			Err(_) => return // FIXME
		}
	}
}
#[test]
fn test_uint32_handler_handle() {
	let x1 = std::cell::Cell::new(0u32);
	let mut h = Uint32Handler{callback: &|y: u32| x1.set(y)};
	let bytes = [255, 1];//, 255, 255, 255, 255, 255, 255];
	h.handle(&mut &bytes[..]);
	assert_eq!(255, x1.get())
	//assert_eq!(std::u32::MAX, x1.get())
}
//struct BoolHandler {
//	callback: Fn(bool)
//}
//struct FloatHandler {
//	callback: Fn(f32)
//}
//struct DoubleHandler {
//	callback: Fn(f64)
//}
//struct BinaryHandler {
//	callback: Fn(&[i8])
//}
//struct VariError<T, E> {
//	value: T,
//	reason: E
//}

#[derive(Debug)]
enum ReadStatus {
	Overflow,
	Empty,
	IO(std::io::ErrorKind)
}

// read_varu64 reads an unsigned varint from the reader r.
fn read_varu64(r: &mut Read) -> Result<u64, ReadStatus> {// (uint64, error) {
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
//fn read_bytes<'a>(r :&'a mut Read) -> &'a [u8] {
//	match read_varu64(r) {
//		Ok(v) => ,
//		Err(e) => Err(e)
//	}
//}

//fn read_vari64(r: &mut Read) -> Result<i64, i64> {
//	let ux = match read_varu64(r) {
//		Ok(v) => v,
//		Err(e) => e
//	};
//	let x = ux as i64;// i64::from(ux >> 1);
	// if ux & 1 != 0 {
	//	x = ^x;
	//}
//	return Ok(x)
//}
/*
	typ := b[0] & 0x07
*/

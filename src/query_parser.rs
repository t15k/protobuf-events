// //////////////////////////
// Scanning
// -------------------------
fn symbol_scan_for_end(s: &str) -> Option<(usize, &str)> {
    for (i, v) in s.char_indices() {
        if v == '.' {
            return Option::Some((i, &s[0..i]))
        }
    }
    if s.len() > 0 {
        Option::Some((s.len(), s))
    } else {
        Option::None
    }
}

//fn op_scan_for_end(s: &[u8]) {
//
//}

fn read_first_items(s: &str) -> Option<(usize, &str)>{
    match s.chars().next() {
        Some(c) => match c {
            // accessor, always one char
            '.' => Some((1, &s[..1])),
            // TODO should test for character
            _ => if c.is_alphabetic() {
                symbol_scan_for_end(s)
            } else {
                // Handle white space
                None
            }                
        }
        None => None
    }
}


pub struct Scanner<'a> {
//    index :usize,
    value :&'a  str
}
 

impl Scanner<'_> {
    // Example .name

    pub fn new<'a>(val :&'a str) -> Scanner<'a> {
        return Scanner{ value: &val[..] }
    }
}

impl<'a> Iterator for Scanner <'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        match read_first_items(self.value) {
            Some((ndx, st)) => {
                self.value = &self.value[ndx..];
                Some(st)},
            _ => None
        }
    }
}

/// Do nothing.
pub fn parse(txt: &String) -> Tokenizer {
    Tokenizer::new(txt)    
}
// ///////////////////////
// Tokenizer
// -----------------------
pub enum Token {
    Accessor,
    Name(String)
}

pub struct Tokenizer<'a>  {
    scanner: Scanner<'a>
}

impl<'a> Tokenizer<'a> {

    pub fn new(v: &str) -> Tokenizer {
        Tokenizer{scanner: Scanner::new(v)}
    }

}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.scanner.next() {
            Some(s) => {
                match s {
                    "." => Some(Token::Accessor),
                    _ => Some(Token::Name(String::from(s)))
                }
            },
            None => None
        }
    }
}

// ////////////////////////
// Tests
//------------------------

mod test {
    #[test]
    fn test_parser() {
        let mut t = super::Scanner::new("asd");
        assert!("asd".eq(t.next().unwrap()));
        assert!(1 == 1);
    }

    #[test]
    fn test_scanner() {
        let mut t = super::Scanner::new(".some_name.y");
        assert_eq!(".", t.next().unwrap());
        assert_eq!("some_name", t.next().unwrap());
        assert_eq!(".", t.next().unwrap());
        assert_eq!("y", t.next().unwrap());
    }

    #[test]
    fn test_symbol_scan_for_end() {
        let s = "name.a";
        let (i_1, s_1) = super::symbol_scan_for_end(&s).unwrap();
        assert_eq!(4, i_1);
        assert_eq!("name", s_1);
        let (i_2, s_2) = super::symbol_scan_for_end(&s[i_1+1..]).unwrap();
        assert_eq!(1, i_2);
        assert_eq!("a", s_2);        
    }
    #[test]
    fn test_read_first_items() {
        let (_, c) = super::read_first_items(".").unwrap();
        assert_eq!(".",  c);
        let (_, c) = super::read_first_items("name").unwrap();
        assert_eq!("name",  c);
        let (_, c) = super::read_first_items(".name").unwrap();
        assert_eq!(".",  c);
    }

    #[test]
    fn test_tokenizer() {
        let mut tokenizer = super::Tokenizer::new(".some.name");
        match tokenizer.next() {
            Some(super::Token::Accessor) => (),
            _ => panic!("Expected Token::Accessor")
        };
        match tokenizer.next() {
            Some(super::Token::Name(v)) => assert_eq!("some", v),
            _ => panic!("Expected Token Name 'some'")
        }
        match tokenizer.next() {
            Some(super::Token::Accessor) => (),
            _ => panic!("Expected Token::Accessor")
        };
        match tokenizer.next() {
            Some(super::Token::Name(v)) => assert_eq!("name", v),
            _ => panic!("Expected Token Name 'name'")
        }
    }
}

//use query_parser;
use protobuf_events;

enum Operand {
    FieldAccessor(protobuf_events::SchemaField),
    MapAccessor(), // (protobuf_events::SchemaField)
}

pub struct Engine {

}


impl Engine {
    pub fn from_text(text :&String) -> Engine {
        Engine{}
    }

    pub fn accept(&self, _: i32) { // TODO add output

    }
}
//<Item=&'a protobuf_events::WireField>
pub struct EngineIterator<T> { //: Iterator<Item=&'a protobuf_events::WireField>
    //fields : &'a [protobuf_events::SchemaField],
    field: protobuf_events::SchemaField,
    wire_fields: T // &'a dyn Iterator<Item=&'a protobuf_events::WireField>
}

impl<'a, T: Iterator<Item=&'a protobuf_events::WireField>> Iterator for EngineIterator<T> { // <Item=&'a protobuf_events::WireField>
    type Item = &'a protobuf_events::WireField;

    fn next(&mut self) -> Option<Self::Item> {
        access_field(&self.field, &mut self.wire_fields)
    }
}

fn dispatch<'a> (operands: &mut impl Iterator<Item=&'a Operand>, fields: &mut impl Iterator<Item=&'a protobuf_events::WireField>)
{
    match operands.next() {
        Some(operand) => {
            match operand {
                // Return iterator for that field along
                Operand::FieldAccessor(x) => {access_field(x, fields)}, // panic!("Arrgh!"); 
//                query_parser::Token::MapAccessor => access_map(fields),
//                query_parser::Token::Name(_) => EngineIterator{}
                _ => panic!("Arrgh!")
            }
        },
        None => panic!("Arrgh!")
    };
}

/// Will read a single field.
pub fn access_field<'a>(
    schema_field: &protobuf_events::SchemaField,
    wire_fields: &mut impl Iterator<Item=&'a protobuf_events::WireField>)
    -> Option<&'a protobuf_events::WireField> {
    
    //for wire_field in wire_fields {
    let wire_field_opp = wire_fields.next();
    match wire_field_opp {
        Some(wire_field) => 
        match wire_field {
            protobuf_events::WireField::VarInt(field_number, value) => {
                if *field_number == schema_field.field_number {                    
                    return Some(wire_field)
                }
            }
            _ => return None
        },
        _ => return None
    }
    None
}

/// Answers the value of the found in given stream.
/// The stream must be pointing at an object with a list of key value pairs
fn access_map(r: &mut dyn Iterator<Item=protobuf_events::WireField>) -> Option<protobuf_events::WireField> {
    None
}
mod test {
    #[test]
    fn test_engine_accept() {

    }

    #[test]
    fn test_dispatch() {
        let wire_fields = &[super::protobuf_events::WireField::VarInt(1, 2)]; 
        let ops = &[super::Operand::FieldAccessor(
            super::protobuf_events::SchemaField{
                field_number: 1,
                kind: super::protobuf_events::SchemaType2::Int32,
                name: String::from("Hello"),
                repeated: false
            })];
        let mut o = ops.iter();
//        let mut xx: impl Iterator<Item=super::Operand> = ops.iter();
        super::dispatch(&mut ops.iter(), &mut (&wire_fields).iter());
    }

    #[test]
    fn test_arrays() {
        let a = [super::Operand::FieldAccessor(
                super::protobuf_events::SchemaField{
                field_number: 1,
                kind: super::protobuf_events::SchemaType2::Int32,
                name: String::from("Hello"),
                repeated: false
                })];
        call_me(60000, 5000, 100012304);

    }

    fn call_me(i: u32, n1: i32, n2: i32) {
        if i > 0 {
            call_me(i-1, n1, n2);
        }
    }

    #[test]
    fn test_field_accessor() {
        let wire_fields = &[super::protobuf_events::WireField::VarInt(1, 150)]; 
        let mut value = 1000;
        for it in super::access_field(
            &super::protobuf_events::SchemaField{
                field_number: 1,
                kind: super::protobuf_events::SchemaType2::Int32,
                name: String::from("Hello"),
                repeated: false
            }, &mut (&wire_fields).iter()) {
            value = match it {
                super::protobuf_events::WireField::VarInt(_, v) => *v,
                _ => 0
            }
        }
        assert_eq!(value, 150);
    }

    struct MyStruct {
        operand: super::Operand,
    }

    #[test]
    fn test_inner_enum() {
        let a = [MyStruct{operand: super::Operand::FieldAccessor(super::protobuf_events::SchemaField{
                field_number: 1,
                kind: super::protobuf_events::SchemaType2::Int32,
                name: String::from("Hello"),
                repeated: false
            })}];
        for item in a.iter() {
            match item.operand {
                super::Operand::FieldAccessor(_) => (),
                _ => ()
            }
        }
    }
    #[test]
    fn test_outer_enum() {
        let a = [super::Operand::FieldAccessor(super::protobuf_events::SchemaField{
                field_number: 1,
                kind: super::protobuf_events::SchemaType2::Int32,
                name: String::from("Hello"),
                repeated: false
        })];
        for item in a.iter() {
            match item {
                super::Operand::FieldAccessor(_) => (),
                _ => ()
            }
        }
    }
}
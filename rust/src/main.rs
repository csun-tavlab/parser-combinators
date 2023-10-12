use std::marker::PhantomData;

enum Term<T> {
    IntTerm { value: i32 },
    PlaceholderTerm { value: u64 },
    StructureTerm { name: String, subterms: Vec<Term<_>> }
}
trait AbstractTerm {}

trait Term<T> : AbstractTerm {}
struct IntTerm { value: u32 }
impl Term<u32> for IntTerm {}
impl AbstractTerm for IntTerm {}
struct PlaceholderTerm<T> {
    value: u64,
    phantom: PhantomData<T>
}
impl <T> Term<T> for PlaceholderTerm<T> {}
impl <A> AbstractTerm for PlaceholderTerm<A> {}
struct StructureTerm<T> {
    name: String,
    subterms: Vec<Box<dyn AbstractTerm>>,
    phantom: PhantomData<T>
}
impl <T> Term<T> for StructureTerm<T> {}
impl <A> AbstractTerm for StructureTerm<A> {}

fn main() {
    let t1 = IntTerm { value: 2 };
    let t2 = PlaceholderTerm::<u32> {
        value: 0,
        phantom: PhantomData
    };
    let t3 = StructureTerm::<u32> {
        name: "foo".to_string(),
        subterms: vec![Box::new(t1), Box::new(t2)],
        phantom: PhantomData
    };
}

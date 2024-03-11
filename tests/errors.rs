use bergbuch::parser::{ParseErr, ParseErrType};
use bergbuch::resolver::ResolverErr;
use bergbuch::interpreter::{InterpretErr, Interpreter};

#[test]
fn scopes_separated() {
    let mut interp = Interpreter::new();
    let err = interp.run("

{
    var x = \"foo bar\";
    var y = \"baz\";
    print \"x: \" + x + \" y: \" + y;
}

{
    var x = \"bar\";
    print x;
    var z = x + y;
}
").unwrap_err();

    match err {
        InterpretErr::ResolverErrs(x) => assert!(x.iter().all(|ev| matches!(ev, ResolverErr::UndeclaredVar(_)))),
        x => panic!("{x:?} should not be reported here.")
    }
}


#[test]
fn reference_during_decl() {
    let mut interp = Interpreter::new();
    let err = interp.run("
var a = \"foo\";
var a = a;
").unwrap_err();

    match err {
        InterpretErr::ResolverErrs(x) => assert!(x.iter().all(|ev| matches!(ev, ResolverErr::UseWhileDeclaring))),
        x => panic!("{x:?} should not be reported here.")
    }
}



#[test]
fn this_not_lvalue() {
    let mut interp = Interpreter::new();
    let err = interp.run("
var result;
class Book {
    setName() {
        this = \"Crafting Interpreters\";
    }
}
");

    match err {
        Err(InterpretErr::ParseErr(x)) => assert!(matches!(x.data, ParseErrType::NotLvalue)),
        x => panic!("{x:?} should not be reported here.")
    }
}

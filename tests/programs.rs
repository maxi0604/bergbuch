use bergbuch::interpreter::Interpreter;
use bergbuch::expr::Val;

#[test]
fn volume() {
    let mut interp = Interpreter::new();
    interp.run("
// How loud?
var volume = 11;

// Silence.
volume = 0;

// Calculate size of 3x4x5 cuboid.
{
  var volume = 3 * 4 * 5;
  print volume;
}
");

    assert_eq!(interp.get_global("volume"), Some(Val::Num(0.0)));
}

#[test]
fn globals() {
    let mut interp = Interpreter::new();
    interp.run("
var result;
var global = \"outside\";
{
  var local = \"inside\";
  result = global + local;
}");

    assert_eq!(interp.get_global("result"), Some(Val::String("outsideinside".into())));
}


#[test]
fn if_else_true() {
    let mut interp = Interpreter::new();
    interp.run("
var result;
if (true or false) {
    result = \"foo\";
} else {
    result = \"bar\";
}
");

    assert_eq!(interp.get_global("result"), Some(Val::String("foo".into())));
}


#[test]
fn if_else_false() {
    let mut interp = Interpreter::new();
    interp.run("
var result;
if (true and false) {
    result = \"foo\";
} else {
    result = \"bar\";
}
");

    assert_eq!(interp.get_global("result"), Some(Val::String("bar".into())));
}


#[test]
fn comment() {
    let mut interp = Interpreter::new();
    interp.run("
var result;
// result = 42;
result = 6;
");

    assert_eq!(interp.get_global("result"), Some(Val::Num(6.0)));
}

#[test]
fn simple_function() {
    let mut interp = Interpreter::new();
    interp.run("
fun foo(bar) {
    var a = 1;
    if (a > 1) {
        a = a + 1;
    }
    return bar + a;
}

var result = foo(1);
");

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}


#[test]
fn return_local_function() {
    let mut interp = Interpreter::new();
    interp.run("
fun returnsFun() {
    fun inner(foo) {
        return foo + 1;
    }

    return inner;
}

var result = returnsFun()(1);
");

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}

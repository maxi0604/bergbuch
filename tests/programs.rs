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
fn if_elseif_else() {
    let mut interp = Interpreter::new();
    interp.run("
var result;
if (true and false) {
    result = \"foo\";
} else if (false or true) {
    result = \"far\";
} else {
    result = \"bar\";
}
");

    assert_eq!(interp.get_global("result"), Some(Val::String("far".into())));
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
fn simple_while() {
    let mut interp = Interpreter::new();
    interp.run("
var a = 0;
while (a < 10) {
a = a + 1;
}
");

    assert_eq!(interp.get_global("a"), Some(Val::Num(10.0)));
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

#[test]
fn closure() {
    let mut interp = Interpreter::new();
    interp.run("
fun returnsFun() {
    var a = 1;
    fun inner(foo) {
        return foo + a;
    }

    return inner;
}

var result = returnsFun()(1);
");

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}

#[test]
fn leaky_closure() {
    let mut interp = Interpreter::new();
    interp.run("
var result1;
var result2;
var a = \"global\";
{
  fun showA() {
    return a;
  }

  result1 = showA();
  var a = \"block\";
  result2 = showA();
}
");

    assert_eq!(interp.get_global("result1"), Some(Val::String("global".into())));
    assert_eq!(interp.get_global("result2"), Some(Val::String("global".into())));
}

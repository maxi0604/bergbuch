use bergbuch::expr::Val;
use bergbuch::interpreter::Interpreter;

#[test]
fn volume() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
// How loud?
var volume = 11;

// Silence.
volume = 0;

// Calculate size of 3x4x5 cuboid.
{
  var volume = 3 * 4 * 5;
  print volume;
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("volume"), Some(Val::Num(0.0)));
}

#[test]
fn globals() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
var global = \"outside\";
{
  var local = \"inside\";
  result = global + local;
}
",
        )
        .unwrap();

    assert_eq!(
        interp.get_global("result"),
        Some(Val::String("outsideinside".into()))
    );
}

#[test]
fn if_else_true() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
if (true or false) {
    result = \"foo\";
} else {
    result = \"bar\";
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::String("foo".into())));
}

#[test]
fn if_else_false() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
if (true and false) {
    result = \"foo\";
} else {
    result = \"bar\";
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::String("bar".into())));
}

#[test]
fn if_elseif_else() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
if (true and false) {
    result = \"foo\";
} else if (false or true) {
    result = \"far\";
} else {
    result = \"bar\";
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::String("far".into())));
}

#[test]
fn comment() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
// result = 42;
result = 6;
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(6.0)));
}

#[test]
fn simple_function() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
fun foo(bar) {
    var a = 1;
    if (a > 1) {
        a = a + 1;
    }
    return bar + a;
}

var result = foo(1);
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}

#[test]
fn simple_while() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var a = 0;
while (a < 10) {
a = a + 1;
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("a"), Some(Val::Num(10.0)));
}
#[test]
fn return_local_function() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
fun returnsFun() {
    fun inner(foo) {
        return foo + 1;
    }

    return inner;
}

var result = returnsFun()(1);
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}

#[test]
fn closure() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
fun returnsFun() {
    var a = 1;
    fun inner(foo) {
        return foo + a;
    }

    return inner;
}

var result = returnsFun()(1);
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(2.0)));
}

#[test]
fn leaky_closure() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
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
",
        )
        .unwrap();

    assert_eq!(
        interp.get_global("result1"),
        Some(Val::String("global".into()))
    );
    assert_eq!(
        interp.get_global("result2"),
        Some(Val::String("global".into()))
    );
}

#[test]
fn fib() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
var result = fib(10);
var after = clock();
print after - before;
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(55.0)));
}

#[test]
fn simple_class() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
class Book {
    name() {
        result = \"Crafting Interpreters\";
    }
}

var resultBefore = result;
var book = Book();
book.name();
",
        )
        .unwrap();

    assert_eq!(interp.get_global("resultBefore"), Some(Val::Nil));
    assert_eq!(
        interp.get_global("result"),
        Some(Val::String("Crafting Interpreters".into()))
    );
}

#[test]
fn simple_for() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result = 0;
for (var i = 0; i < 10; i = i + 1)
    result = result + i;
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(45.0)));
}

#[test]
fn for_with_only_cond() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result = 0;
var i = 0;
for (; i < 10;) {
    result = result + i;
    i = i + 1;
}
",
        )
        .unwrap();

    assert_eq!(interp.get_global("result"), Some(Val::Num(45.0)));
}

#[test]
fn this_reference() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
class Book {
    setName() {
        this.name = \"Crafting Interpreters\";
    }
}

var resultBefore = result;
var book = Book();
book.setName();
result = book.name;
",
        )
        .unwrap();

    assert_eq!(interp.get_global("resultBefore"), Some(Val::Nil));
    assert_eq!(
        interp.get_global("result"),
        Some(Val::String("Crafting Interpreters".into()))
    );
}


#[test]
fn inherit_method() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result;
class A {
    method() {
        result = 42;
    }
}

class B < A {
}
var b = B();
b.method();
",
        )
        .unwrap();

    assert_eq!(
        interp.get_global("result"),
        Some(Val::Num(42.0))
    );
}

#[test]
fn super_access() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var step1;
var step2;
class Doughnut {
  cook() {
    step1 = \"Fry until golden brown.\";
  }
}

class BostonCream < Doughnut {
  cook() {
    super.cook();
    step2 = \"Pipe full of custard and coat with chocolate.\";
  }
}

BostonCream().cook();
",
        )
        .unwrap();

    assert_eq!(
        interp.get_global("step1"),
        Some(Val::String("Fry until golden brown.".into()))
    );
    assert_eq!(
        interp.get_global("step2"),
        Some(Val::String("Pipe full of custard and coat with chocolate.".into()))
    );
}

#[test]
fn returns_function() {
    let mut interp = Interpreter::new();
    interp
        .run(
            "
var result1;
var result2;

fun returnsClass() {
    class Classy {
        init() {
            result1 = 1;
        }

        test() {
            result2 = 2;
        }
    }
    return Classy;
}

var elem = returnsClass()();
elem.test();
",
        )
        .unwrap();

    assert_eq!(
        interp.get_global("result1"),
        Some(Val::Num(1.0))
    );
    assert_eq!(
        interp.get_global("result2"),
        Some(Val::Num(2.0))
    );
}


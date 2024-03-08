use bergbuch::interpreter::Interpreter;
use bergbuch::expr::Val;
#[test]
fn volume() {
    let mut interp = Interpreter::new();
    interp.run("// How loud?
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

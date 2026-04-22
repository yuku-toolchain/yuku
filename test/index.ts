// simple class extends with type arguments
class C extends A<T> {}

// class extends with type arguments and implements
class D extends A<T> implements B<T> {}

// super class is a dotted path with type arguments
class E extends Foo.Bar<T, U> {}

// super class is a call expression followed by type arguments
class F extends getSomething()<number, string> {}

// generic class extends generic super class
class G<T> extends Base<T> {}

// declare class with super type arguments
declare class H<T> extends Base<T> {}

// abstract class with super type arguments
abstract class I<T> extends Base<T> implements X<T>, Y<T> {}

// no type arguments - negative check
class J extends Base {}

// empty type argument list is not valid syntax, but `<>` would error cleanly
// class K extends Base<> {} // intentionally omitted

// nested generics
class L extends Map<string, Array<number>> {}

// multiline
class M<T>
  extends Base<T>
  implements Foo<T>
{
  x: T;
}

new <a/>();
class A extends <a/> {}
new <></>();
class B extends <></> {}

new (<a/>)();
class C extends (<a/>) {}
new <a/>.constructor(1);
class D extends <a/>.constructor {}
new <a></b>();
class E extends <a></b> {}
const after = <a/>;
const arrow = <T,>(value: T) => value;

class Missing extends <a>

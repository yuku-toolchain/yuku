// expression type arguments close only with a lone `>`, fused closers keep
// the operator parse
fn(x < y, x >= y);
f = h >>> 0 < j >>> 0;
g = a<b>=c;
r = f<T>>x;
inst = a?.b<c>;
called = a?.b<c>();
nested = f<A<B<C>>>(x);
lit = 0<T>(1);
tagged = new C<T>``;
constructed = new C<T>`x`(1);
plain = new C<T>;

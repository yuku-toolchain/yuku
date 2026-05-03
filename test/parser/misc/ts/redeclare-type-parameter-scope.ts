// Valid: same name in nested type-parameter scopes (each scope is fresh).

// outer T shadowed by method T
class S1<T> {
  m<T>(x: T): T {
    return x;
  }
}

// outer T shadowed by inner function T
function s2<T>() {
  function inner<T>(x: T): T {
    return x;
  }
  return inner;
}

// type parameter shadowing an outer interface of the same name
interface S3 {}
function s3<S3>(x: S3): S3 {
  return x;
}

// type parameter shadowing an outer class of the same name
class S4 {}
function s4<S4>(x: S4): S4 {
  return x;
}

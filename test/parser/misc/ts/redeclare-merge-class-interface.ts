// Valid: class + interface merge (interface adds members to class instances).
class C1 {
  x: number;
}
interface C1 {
  y: string;
}

// Reverse order also valid.
interface C2 {
  y: string;
}
class C2 {
  x: number;
}

interface User {
  name: string;
  age: number;
  role: string;
}

function summarize({ name, age, role = "guest" }: User): string {
  return `${name}:${age}:${role}`;
}

const me = { name: "alice", age: 30 } as User;

export function run() {
  return {
    explicit: summarize({ name: "bob", age: 5, role: "admin" }),
    defaulted: summarize(me),
    objectLiteralShorthand: ((): unknown => {
      const name = "carol";
      const age = 7;
      return { name, age };
    })(),
  };
}

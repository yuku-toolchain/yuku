export function run() {
  function outer(input) {
    const firstName = input.first;
    const lastName = input.last;
    function inner() {
      const age = 30;
      const city = "nyc";
      return [firstName, age, city];
    }
    return [inner(), lastName];
  }
  return outer({ first: "alice", last: "smith" });
}

function testFunction() {
  const unclosedString = "
  function innerFunction() {
    10 = 10;
  }
  break;
}

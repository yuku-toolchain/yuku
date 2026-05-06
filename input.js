function sayHello() {
  let name = prompt("What is your name?");

  if (name) {
    alert("Hello, " + name + "! Welcome to the world of coding.");
    console.log("User name is: " + name);
  } else {
    alert("Hello, stranger!");
  }
}

sayHello();

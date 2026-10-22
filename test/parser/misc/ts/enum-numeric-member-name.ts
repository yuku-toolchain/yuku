enum StringName {
  "1" = 1,
  "01" = 2,
  "-0" = 3,
  "NaN" = 4,
  "1e21" = 5,
}

enum ComputedString {
  ["1"] = 1,
  ["-1"] = 2,
  ["1e+21"] = 3,
  ["\x31"] = 4,
  [`1`] = 5,
  ["Infinity"] = 6,
}

enum ComputedNumber {
  [0x10] = 1,
  [.5] = 2,
  [1e999] = 3,
}

enum Parenthesized {
  [("D")] = 1,
}

enum NumericToken {
  1 = 1,
}

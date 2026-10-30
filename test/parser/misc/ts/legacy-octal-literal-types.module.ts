// strict code rejects legacy octal numbers and escapes in types too, as tsc does
type Octal = 01;
type NegativeOctal = -01;
type LeadingZero = 08;
type NegativeLeadingZero = -08;
type OctalEscape = "\01";

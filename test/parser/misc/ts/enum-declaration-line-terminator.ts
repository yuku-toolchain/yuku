// enum is unconditionally reserved so its name and body may sit on new lines,
// while type, interface, and namespace still require a same-line name
declare enum
E
{}

enum
F
{
    A,
    B = 2,
}

const enum
G
{}

type
x = 1

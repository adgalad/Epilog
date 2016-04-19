# EPILOG Programming Language

EPILOG is a general purpose programming language developed for the elective
courses on Programming Languages at Universidad Simón Bolívar. It is inspired,
in part, by the syntax of programming languages Prolog[^1]
and [Erlang](http://www.erlang.org/).

EPILOG is imperative and strongly typed. It supports functions, procedures,
logical (if) and value (switch) selectors, count- and condition-controlled
loops, recursion, line and block comments, as well as arbitrarily nested
structured (record) and union (either) type definition.

## Lexical considerations

### Keywords

The following are keywords. This means they are all reserved and they cannot be
used as identifiers nor redefined.

`boolean`, `character`, `either`, `end`, `false`, `finish`, `float`, `for`,
`function`, `if`, `integer`, `is`, `otherwise`, `print`, `procedure`, `read`,
`record`, `return`, `string`, `true`, `void`, `while`

The following are operator and punctuation marks.

`+`, `-`, `*`, `/`, `%`, `and`, `or`,`|`, `!|`, `<`, `>`, `=>`, `=<`, `=`, `!=`,
`:-`, `->`, `,`, `.`, `;`, `_`

### Identifiers

An identifier is a sequence of letters (`[A-Za-z]`) and digits (`[0-9]`) of any
length, starting with a letter, which isn't a keyword. Identifiers are
classified into Variable Identifiers and General Identifiers.

Variable Identifiers must begin with a capital letter (`[A-Z]`), and are used
for naming variables.

General Identifiers must begin with a lowercase letter, and are used for
naming functions, procedures, structures and union types.

### Whitespace

Whitespace (i.e. spaces, tabs, newlines) serves to separate tokens and is
otherwise ignored. Keywords and identifiers must be separated by whitespace or
a token that is neither a keyword nor an identifier. `returniftrue` is a single
identifier, not three keywords; while `if(23this` scans as four tokens.

### Constants

A boolean constant is either of the keywords `true` or `false`.

A char constant is specified as `'<char>'`, where `<char>` is either a
printable ascii character (An upper or lowercase letter, a digit, the space
character, or a symbol different from `\`, `'`, or `"`), or an escape
sequence, which is evaluated according to the following table:

| Escape Sequence | Character Represented |
|-----------------|-----------------------|
| `\n`            | Newline               |
| `\t`            | Horizontal Tab        |
| `\\`            | Backslash             |
| `\'`            | Single quotation mark |
| `\"`            | Double quotation mark |

An integer constant can either be specified in decimal (base 10) or hexadecimal
(base 16). A decimal integer is a sequence of decimal digits (`[0-9]`). A
hexadecimal integer must begin with `0X` or `0x` (that is a zero, not the
letter oh) and is followed by a sequence of hexadecimal digits. Hexadecimal
digits include the decimal digits and the letters `a` through `f` (either upper
or lowercase). Examples of valid integers: `8`, `012`, `0x0`, `0X12aE.

A float constant is a sequence of digits, a period, followed by any sequence of
digits, at least one. Thus, neither `.12` nor `12.` are valid floats, but both
`0.12` and `1.2` are valid. A float can also have an optional exponent, e.g.,
`12.2E+2` For a float in this sort of scientific notation, the decimal point
is required, the sign of the exponent is optional (if not specified, `+`
is assumed), and the `E` can be lower or upper case. As above, `.12E+2` and
`12.E+2` are invalid, but `1.2E+2` is valid. Leading zeroes on the mantissa
and exponent are allowed.

A string constant is a sequence of characters enclosed in double quotes (`"`).
Strings can contain any character except a newline or double quote. A string
must start and end on a single line, it cannot be split over multiple lines.

Examples:

~~~erlang
boolean aBoolean is true,

character aCharacter is 'a',
character anotherCharacter is '\n',

integer aInteger is 42,
integer anotherInteger is 0x2A,

float aFloat is 4.2,
float anotherFloat is 6.02E23,
float yetAnotherFloat is 1.0E-42,

string aString is "Privyet Mir!"
string anotherString is "This is my new string\nA haiku with two new lines\nRefrigerator"
~~~

### Comments

EPILOG allows for both single line comments as well as block comments.
Examples:

~~~erlang
int X, %% This is a single line comment

/%
 % While this is
 % a block comment.
 %/ 
~~~

### Operators

EPILOG's operators have the following procedence, from highest to lowest:

|    Operator      |                    Description                 | Associativity |
|------------------|------------------------------------------------|---------------|
| `_`              | Access to record element                       | Left to right |
| `-`, `!`         | Unary arithmetic and logical negation          | Right to left |
| `*`, `/`, `%`    | Multiplicative                                 | Left to right |
| `-`, `+`         | Additive                                       | Left to right |
| `<`,`=<`,`>`,`>=`| Relational                                     | Left to right |
| `and`, `or`      | Conjunction, disjunction                       | Left to right |
| `=`, `!=`        | Equality                                       | Left to right |

A boolean expression using logical AND and OR operator has short circuit evaluation. It means, 
EPILOG dos not evaluate an operand unless it is neccessary to resolve the result of the expression.


# References

[^1]: Prolog has quite a few different compilers, the most widely
used of which are [GNU Prolog](http://www.gprolog.org/),
[SWI-Prolog](http://www.swi-prolog.org/) and
[SICStus](http://www.sics.se/sicstus/).

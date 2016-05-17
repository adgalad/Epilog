# Epilog Programming Language
Epilog is a general purpose programming language developed for the elective
courses on Programming Languages at Universidad Simón Bolívar. It is inspired,
in part, by the syntax of programming languages Prolog[^1]
and [Erlang](http://www.erlang.org/).

Epilog is imperative and strongly typed. It supports procedures, logical (if)
and value (case) selectors, count- (for) and condition-controlled (while) loops,
recursion, line and block comments, as well as arbitrarily nested structured
(record) and union (either) type definition.


## Lexical considerations

### Keywords
The following are keywords. This means they are all reserved and they
cannot be used as identifiers nor redefined.

> `and`, `andalso`, `band`, `bnot`, `boolean`, `bor`, `bsl`, `bsr`,
> `bxor`, `character`, `div`, `either`, `end`, `false`, `finish`,
> `float`, `for`,  `if`, `integer`, `is`, `length`, `not`, `or`,
> `orelse`, `otherwise`, `print`, `procedure`, `read`, `record`,
> `rem`, `return`, `string`, `toBoolean`, `toCharacter`, `toFloat`,
> `toInteger`, `true`, `void`, `while`, `xor`

### Identifiers
An identifier is a sequence of letters (`[A-Za-z]`) and digits (`[0-9]`) of any
length, starting with a letter, which isn't a keyword. Identifiers are
classified into Variable Identifiers and General Identifiers.

Variable Identifiers must begin with a capital letter (`[A-Z]`), and are used
for naming variables.

General Identifiers must begin with a lowercase letter, and are used for naming
procedures, structures and union types.

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
| `\0`            | Null character        |
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
    boolean ABoolean is true,

    character ACharacter is 'a',
    character AnotherCharacter is '\n',

    integer AInteger is 42,
    integer AnotherInteger is 0x2A,

    float AFloat is 4.2,
    float AnotherFloat is 6.02E23,
    float YetAnotherFloat is 1.0E-42,

    string AString is "Privyet Mir!",
    string AnotherString is "This is my new string\nA haiku with two newlines\nRefrigerator".
~~~

### Operators
The operators and punctuation characters used in Epilog include

> `(`, `)`, `*`, `+`, `,`, `-`, `.`, `/=`, `/`, `[`, `]`, `{`, `}`, `<`, `=<`,
> `=`, `>=`, `>`, `_`, `|`, `!|`, `and`, `andalso`, `band`, `bnot`, `bor`,
> `bsl`, `bsr`, `bxor`, `div`, `is`, `length`, `not`, `or`, `orelse`, `rem`, `xor`

Operators have the following precedence, from highest to lowest:

|    Operator            |                    Description                  | Associativity |
|------------------------|-------------------------------------------------|---------------|
| `to*`                  | Type conversion operators                       | Right to left |
| `length`               | Array length                                    |      None     |
| `[ x }` `{ x ]`        | Array subscripting                              | Left to right |
| `_`                    | Record entry and union member access            | Left to right |
| `-`, `not`, `bnot`     | Unary arithmetic, logical, and bitwise negation | Right to left |
| `*`, `/`, `div`, `rem` | Multiplicative                                  | Left to right |
| `+`, `-`               | Additive                                        | Left to right |
| `bsl`, `bsr`           | Bitwise left shift and right shift              | Left to right |
| `<`,`=<`,`>`,`>=`      | Relational                                      |      None     |
| `|` `!|`               | "divisor of" operator and its negation          |      None     |
| `=`, `/=`              | Equality                                        | Left to right |
| `band`                 | Bitwise AND                                     | Left to right |
| `bxor`                 | Bitwise XOR                                     | Left to right |
| `bor`                  | Bitwise OR                                      | Left to right |
| `and`                  | Logical AND                                     | Left to right |
| `xor`                  | Logical XOR                                     | Left to right |
| `or`                   | Logical OR                                      | Left to right |
| `andalso`              | Short-circuit conjunction                       | Left to right |
| `orelse`               | Short-circuit disjunction                       | Left to right |
| `is`                   | Simple assignment                               | Right to left |

The operators `orelse` and `andalso` only evaluate their second argument if
the first one doesn't provide enough information to determine the value of the
full expression. Specifically, in `<exp_1> orelse <exp_2>`, `<exp_2>` is only
evaluated if `<exp_1>` evaluates to false, since otherwise the value of the
expression would just be true. The same logic applies, *mutatis mutandis*, to
the `andalso` operator.

The operators `[ x }` and `{ x ]` take an array on their lefts and an integer
expression on between in the place of the `x` and return the element type of
the array. They work identically.

The family of operators `to*`, that is, `toBoolean`, `toCharacter`, `toFloat`,
and `toInteger`, take an expression of any type and return the converted
value in the corresponding type.

The operator `length` takes an array of any type and returns its length, an
integer.

The operator `_` takes a record or either type on its left and a name on its
right and returns the named entry or member of the record or either.

The operators `+`, `*`, and `-`, both unary and binary, work on floats and
on integers, taking two values of the same type and returning a third one of
the same.

The operator `/` only works between floats and returns floats.

The operators `not`, `and`, `andalso`, `xor`, `or`, and `orelse` only work
between booleans and return booleans.

The operators `bnot`, `div`, `rem`, `bsl`, `bsr`, `band`, `bxor` and `bor`
only work between integers, and return integers.

The operator `|` only works between integers and returns a boolean.

The operators `<`, `=<`, `>`, and `>=` work on chars, floats and integers,
taking two values of the same type and returning a boolean.

The operators `=` and `/=` work on all types, taking two values of the same
type and returning a boolean.

### Comments
Epilog allows for both single-line comments as well as block comments.
A single-line comment starts with `%%` and extends to the end of the line. A
block comment starts with `/%` and ends with the first subsequent `%/`. Any
symbol is allowed in a comment except the sequence `%/`, which ends the current
comment. Block comments do not nest.

Examples:

~~~erlang
    integer X, %% This is a single line comment

    /%
     % While this is
     % a block comment.
     %/
~~~


## Reference Grammar
[Click here](grammar.md)


## Program Structure

An Epilog program consists of zero or more `record` or `either` definitions, as
well as one or more `procedure` definitions. One of the procedure definitions
must be for the special procedure `main`. This procedure will be the first one
to be executed when running the compiled Epilog program, and will be the place
from which to call any other procedures declared in the program, as well as make
use of the defined `record`s and `either`s.

Example:

~~~erlang
    record myRecord :-
        integer MyInteger,
        float   MyFloat.

    either myEither :-
        character CharacterMember,
        integer IntegerMember.

    procedure proc(character C) :-
        read(C),
        C is toCharacter(toInteger(C) + 42).

    procedure main() :-
        print("This is the first instruction"),

        myEither D,
        proc(D_CharacterMember),
        print(D_IntegerMember).
~~~

## Scoping

Epilog uses a static scope and support nested scopes. Each `procedure` has its
own scope for its parameters and the variables declared inside them. Every block
inside a control structure creates a new scope too.

- Any identifier declared at the highest scope is a global identifier.
- Every identifier must be declared before it can be used.
- Procedures can only be declared at the highest scope.
- Declaring an identifier twice in the same scope is not allowed.
- A variable identifier can be redefined in a nested scope and it hides or
    "shadows" the previous declaration until the scope is left.

## Variables

Each variable must be declared with its type. Variables can have any of the
scalar types given by the language, except void, or any of the composite type
declared by the user. A variable can also be an array of the above types.
To declare a variable use the following syntax:

~~~erlang
    <type> <variable_id>.
~~~

To assign a value to a variable the assignment operator `is` is used. If a
variable is not initialized, it contains whatever the memory contained at the
moment of the declaration; in other words, its value is undefined.

Example:
~~~erlang
    integer Xa is 5,
    integer Xb is 6,
    integer Xc is Xa+Xb.
~~~


## Scalar types

### boolean
Takes either of the values `true` or `false`.

### character
Represents one of the 128 ASCII characters.

### integer
Represents signed integers between -2147483648 and 2147483647.

### float
Represents an IEEE-754 32-bit floating point number.


## Composite types

### Arrays
Arrays are homogeneous, linearly indexed collections of a given type and size.
The size of an array must always be an integer known at compilation time.
Epilog arrays are zero-indexed, that is, the first element of an array is
element 0. The declaration of an array obeys the following syntax:

~~~erlang
    <base_type0>[<size>} TheArray.
    <base_type1>{<size>] TheOtherArray.
~~~

Keep in mind that the `<base_type>` may be any native epilog type, any type
defined in the source file, or an array of any of these.

The elements of an array are accessed using the `[ x }` and `{ x ]` operators,
according to the following syntax:

~~~erlang
    TheValue is TheArray[42}.
    TheOtherValue is TheOtherArray{69].
~~~

Both operators have exactly the same meaning, but their simultaneous presence
serves two purposes: it forces Epilog programmers to always check their indices
twice, preventing bugs, and it brings some `sabor latino` into Epilog.

A programmer who uses arrays can take advantage of the predefined `length`
unary operator when writing procedures. This operator returns the length of the
array to its right.

Attempting to access an element outside the bounds of an array produces a
runtime error.

Examples:
~~~erlang
    integer[100} myNums,
    for I from 0 to length myNums - 1 ->
        read(myNums{I])
    end,

    <...>,

    integer Z,
    for I from 0 to length myNums - 1 ->
        Z is Z + myNums{I]
    end,

    print(Z),

    myNums[100} is 42. %% this produces a runtime error.
~~~

### Records
Epilog `record`s are arbitrarily nested structures. This abstraction
allows Epilog programmers to define their own data types by meaningfully
associating other data types, which can be native types, arrays, `either`s
or other `record`s.

It is not possible, however, to define a `record` containing itself, either
directly or indirectly, since this would consume an infinite amount of memory.

All `record` types must be defined in the global scope.

A `record` type is defined with the following syntax:
~~~erlang
    record <record_name> :-
        <type_0> <entry_0>
        [, <type_i> <entry_i>]
    .
~~~

To declare a variable of a `record` type, the usual declaration syntax is used:
~~~erlang
    <record_name> <variable_name>.
~~~

To access the entries of a `record` type, the `_` operator is used, followed
by the name of the entry. The syntax is the following:
~~~erlang
    <record_variable_name>_<entry_name>.
~~~

Accessing an entry which is not in the definition of the `record` produces a
compilation time error.

Examples:
~~~erlang
    record person :-
        string Name,
        integer Age,
        boolean IsRegistered.

    <...>

    person User,
    print("What is your name?"),
    read(User_Name),
    print("How old are you?"),
    read(User_Age),
    User_IsRegistered is True.
~~~

### Eithers
`either` types are structures which might hold a single value at a time. This
allows the programmer to make efficient use of memory when she considers it
necessary, since the same memory address can be used for either of the member
types. The member types of an `either` type can be native types, arrays,
`record`s or other `either`s.

It is not possible, however, to define an `either` containing itself, either
directly or indirectly, since this would stump the compiler indefinitely.

All `either` types must be defined in the global scope.

An `either` type is defined with the following syntax:
~~~erlang
    either <either_name> :-
        <type_0> <member_0>
        [, <type_i> <member_i>]
    .
~~~

To declare a variable of an `either` type, the usual declaration
syntax is used:
~~~erlang
    <either_name> <variable_name>.
~~~

To access the members of an `either` type, the `_` operator is used, followed
by the name of the member. The syntax is the following:
~~~erlang
    <either_variable_name>_<member_name>.
~~~

If the member accessed is not the same as the last member that was assigned,
the value of the resulting value is undefined.

Accessing a member which is not in the definition of the `either` produces a
compilation time error.

Examples:

~~~erlang
    either magic :-
        float Float,
        integer Integer.

    <...>

    float N,

    <...>

    magic Magic,
    Magic_Float is N,
    Magic_Integer is (1 bsl 29) + (Magic_Integer bsr 1) - (1 bsl 22),

    float SqrtN is Magic_Float,

    print(SqrtN).
~~~

## Special Types

### Strings
`string`s are immutable sequences of ASCII characters. They must be
initialized at the site of declaration. The constraints for `string`
constants are discussed in the section "Constants" under
"Lexical considerations".


## Type equivalence and compatibility
Epilog has strict types, which means that at no point are values implicitly
converted or coerced from one type to another. In other words, a type is only
equivalent and compatible with itself. For conversion between native types, the
predefined unary operators `toBoolean`, `toCharacter`, `toInteger` and `toFloat`
are provided, with the following semantics:

| From ↓    | `toBoolean`          | `toChar`                        | `toInteger`                    | `toFloat`                         |
|-----------|----------------------|---------------------------------|--------------------------------|-----------------------------------|
| `boolean` | *                    | `'T'` if `true`, `'F'` if false | `1` if `true`, `0` if `false`  | `1.0` if `true`, `0.0` if `false` |
| `char`    | `true` if not `'\0'` | *                               | padded with zeros              | toFloat . toInteger               |
| `integer` | `true` if not zero   | truncated to 7 bits             | *                              | ***TO DO: define conversion***    |
| `float`   | `true` if not zero   | toChar . toInteger              | ***TO DO: define conversion*** | *                                 |

where the cells marked (*) produce compilation time errors for unnecessary
conversions.

Equivalence between `record` and `either` types is strictly by name, which
means that it is impossible for two values declared as different `record`
types or different `either` types to be equivalent, even if the structures
are the same.

Regarding array types, their equivalence depends completely on the equivalence
of their element types, the only instance of structural equivalence in the
definition of the Epilog language.

## Control Structures

### If
The most basic conditional in Epilog is the `if` statement. It has one or more
guards (boolean expressions), each with an associated branch (one or more
instructions). The guards are evaluated sequentially and when a true one is
found, the associated branch is executed and the rest are skipped. If no true
guard is found, the program continues after the `end` token. If the programmer
wishes to, she can use the keyword `otherwise` in the last guard, similarly to
`else` branches in other programming languages.

Syntax:
~~~erlang
    if
        <guard_0> -> <instruction_00> [, <instruction_0j>]
        [;<guard_i> -> <instruction_i0> [, <instruction_ij]]
    end
~~~

Examples:
~~~erlang
    if greetUser ->
        print("Hi $username!")
    end,

    if
        X > 0 -> print("X is positive."), Positives is Positives + 1;
        X = 0 -> print("X is zero."), Zeroes is Zeroes + 1;
        X < 0 -> print("X is negative."), Negatives is Negatives + 1
    end,

    if
        2 | Y -> print("Y is even."), Z is 2;
        3 | Y -> print("Y is an odd multiple of three.);
        otherwise -> print("Y is a boring number.")
    end.
~~~

### Case
The `case` statement takes an integer expression which is evaluated. It then
looks for the resulting value in the guards, and finally executes the branch
where the value is found. If the value isn't found in any branch, the program
continues after the `end` token. If the programmer wishes to, she can use the
keyword `otherwise` in the last guard, similarly to the `default` case of
other programming languages.

Syntax:
~~~erlang
    case <int_expresion> of
        <int_00> [, <int_0k>] -> <instruction_00> [, <instruction_0j>]
        [;<int_i0> [, <int_ik>] -> <instruction_i0> [, <instruction_ij]]
    end
~~~

Examples:
~~~erlang
    read(I),
    case I of
        2, 3, 5, 7, 11 -> print("I love small primes.");
        4, 8, 16 -> print("Well, at least it's a power of two");
        42 -> print("Six times nine");
        otherwise -> print("Go away"), Away is True
    end.
~~~

### While
The `while` statement allows for conditional iteration based on a boolean
expression. When the statement is first reached, the expression is evaluated,
and if it's `true`, the body is executed. After each execution of the body,
the expression is evaluated again, until it becomes `false`, when the loop
is broken and the first instruction after the `end` token is executed.

Syntax:
~~~erlang
    while <bool_expresion> ->
        <instruction_0> [, <instruction_j>]
    end
~~~

Examples:
~~~erlang
    read(I),
    while I /= 42 ->
        print("Keep trying!"), read(I)
    end,
    print("You found it!").
~~~

### For
The `for` statement allows for count-controlled iteration. When the statement
is first reached, the range is evaluated, the iteration variable is declared
and assigned the first value in the range, and the body is executed. After
the first iteration, the iteration variables is assigned the second value
in the range, and so on, until the last value is reached. Subsequently,
execution proceeds regularly after the `end` token.

If a variable with the same name as the iteration variable had been declared
in the scope of the `for` statement, it is shadowed by the iteration variable.

Syntax:
~~~erlang
    for <variable_name> from <lower_bound> to <upper_bound> ->
        <instruction_0> [, <instruction_j>]
    end
~~~

Examples:
~~~erlang
    for I from 0 to 42 ->
        if
            15 | I -> print("fizzbuzz");
            3  | I -> print("fizz");
            5  | I -> print("buzz");
            otherwise -> print(I)
        end
    end.
~~~


## Expressions
***TO DO: Expressions***


## Procedures
A procedure is used to define a routine, so it can be called at any point of
the code. Procedures are declared using the keyword `procedure` followed
by its name and parameters. Procedures can receive one or more parameters
and always return void. If the programmer wishes to exit from a procedure
before reaching its last line, the keyword `finish` can be used.

Syntax:
~~~erlang
    procedure <procedure_name> (<type_0> <parameter_0> [, <type_i> <parameter_i>]) :-
        <instruction_0> [, <instruction_j>].
~~~

Examples:
~~~erlang
    procedure bar(integer X, float Y) :-
        integer Z is 3,
        X is X + Z,
        Y is Y - 1.0.
~~~

~~~erlang
    procedure q(integer X, integer Y, integer Z) :-
        if Z > 0 ->
            X is X + 10,
            Y is Z + Y,
            Z is Z - 1,
            q(Y,X,Z)
        end.
~~~


## Subroutine invocation

Procedure invocation involves passing the argument values from the caller to the
callee (the subroutine) and executing its body. When a subroutine is invoked,
the actual arguments are evaluated and bound to the formal parameters. All
Epilog parameters and return values are **passed by value**
(***TO DO:*** change this).

- The number of actual arguments in a subroutine call must match the
number of formal parameters.
- The type of each actual argument in a subroutine call must be compatible
with the formal parameter.
- The actual arguments to a subroutine call are evaluated from left to right.
- A procedure call returns control to the caller on a finish statement
or when the textual end of the procedure is reached.
- A procedure call evaluates to the `void` type and cannot be assigned to
a variable.


## Run time checks
- When accessing an element of an array, it is verified that the index value
is between the array's bounds. Attempting to access an element outside the
bounds of the array produces a runtime error.

# References
[^1]: Prolog has quite a few different compilers, the most widely
used of which are [GNU Prolog](http://www.gprolog.org/),
[SWI-Prolog](http://www.swi-prolog.org/) and
[SICStus](http://www.sics.se/sicstus/).


# Acknowledgements
The structure of this document is based, in part, on the specification of the
[Decaf](https://parasol.tamu.edu/courses/decaf/students/decafOverview.pdf)
programming language.

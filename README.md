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

`boolean`, `char`, `either`, `end`, `false`, `finish`, `float`, `for`,
`function`, `if`, `int`, `is`, `otherwise`, `print`, `procedure`, `read`,
`record`, `return`, `string`, `true`, `void`, `while`

### Identifiers

An identifier is a sequence of letters (`[A-Za-z]`) and digits (`[0-9]`) of any
length, starting with a letter, which isn't a keyword. Identifiers are
classified into Variable Identifiers and General Identifiers.

Variable Identifiers must begin with a capital letter (`[A-Z]`), and are used
for naming variables.

General Identifiers must begin with a lowercase letter, and are used for
naming functions, procedures, structures and union types.

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

# References

[^1]: Prolog has quite a few different compilers, the most widely
used of which are [GNU Prolog](http://www.gprolog.org/),
[SWI-Prolog](http://www.swi-prolog.org/) and
[SICStus](http://www.sics.se/sicstus/).

EPILOG Programming Language
===========================

EPILOG is a general purpose programming language developed for the elective
courses on Programming Languages at Universidad Simón Bolívar. It is inspired,
in part, by the syntax of programming languages Prolog[^PrologLinks]
and [Erlang](http://www.erlang.org/).

EPILOG is imperative and strongly typed. It supports functions, procedures,
logical (if) and value (switch) selectors, count- and condition-controlled
loops, recursion, line and block comments, as well as arbitrarily nested
structured and union type definition.

Program
-------

Examples:

~~~erlang
    procedure main() :- 
      int X, read(X),
      int Y, read(Y),

      int Base is max(X, Y),
      int Exponent is min(X,Y),

      print(Base),
      print(Exponent),

      int Z is power(Base, Exponent),

      print(Z),

      while Z > 0 ->
        Z is Z / 2,
        print(Z)
      end,

      int$5 Warr,
      for I in {0..4} ->
        Warr$I is 0
      end,

      intchar K,
      if
        X > Y -> K_I is Y;
        otherwise -> K_C is 'C'
      end

      %% The following will be a runtime error if K_I was set.
      print(K_C),


      %% Given the comment in (*), the syntax kind of allows the following
      %% blocks as well:
      %% (Which I guess would just perform the actions in order.)
      for
        I in {0..5} -> something();
        I in {3..10} -> somethingElse()
      end,

      while
        Z > Y -> something3();
        Y > X -> something4();
        X > Z -> something5()
      end.
~~~

# References

[^PrologLinks]: Prolog has quite a few different compilers, the most widely
used of which are [GNU Prolog](http://www.gprolog.org/),
[SWI-Prolog](http://www.swi-prolog.org/) and
[SICStus](http://www.sics.se/sicstus/).

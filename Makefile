
all: Lexer.hs
	ghc Lexer.hs

Lexer.hs: Lexer.x
	alex Lexer.x

clean:
	rm -v *.o *.hi

purge:
	rm -v Lexer Lexer.hs *.o *.hi
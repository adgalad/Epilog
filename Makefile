

all: Lexer.hs
	ghc Lexer.hs
	rm -v *.o *.hi

Lexer.hs: Lexer.x
	alex Lexer.x

clean:
	rm -v Lexer Lexer.hs *.o *.hi
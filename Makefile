all:
	happy src/parser.y
	ghc -package parsec -dynamic src/bzo.hs src/compiler.hs src/error.hs src/types.hs src/lexer.hs src/tokens.hs src/parser.hs src/syntax.hs
clean:
	cd src; rm *.o *.hi

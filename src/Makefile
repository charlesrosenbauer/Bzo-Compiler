all:
	happy parser.y
	ghc -package parsec -dynamic bzo.hs compiler.hs error.hs types.hs lexer.hs tokens.hs
clean:
	rm *.o *.hi

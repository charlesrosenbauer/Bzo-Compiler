all:
	happy src/parser.y
	alex  src/lexer.x
	ghc -dynamic src/bzo.hs src/compiler.hs src/error.hs src/types.hs src/lexer.hs src/tokens.hs src/parser.hs src/syntax.hs
	mv src/bzo bin/bzo
clean:
	rm src/parser.hs src/lexer.hs
	cd src; rm *.o *.hi
cleanDebug:
	cd src; rm *.o *.hi

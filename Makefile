all:
	ghc -package llvm-general-pure -dynamic src/bzo.hs src/compiler.hs src/types.hs src/lexer.hs src/parser.hs src/tokens.hs src/syntax.hs src/parserrules.hs src/parameters.hs src/configparser.hs src/preprocessor.hs
	mv src/bzo bin/bzo
clean:
	cd src; rm *.o *.hi

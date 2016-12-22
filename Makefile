all:
	ghc -package llvm-general-pure -dynamic src/bzo.hs src/compiler.hs src/error.hs src/types.hs src/lexer.hs src/tokens.hs src/parser.hs src/syntax.hs src/typechecker.hs
	mv src/bzo bin/bzo
clean:
	cd src; rm *.o *.hi

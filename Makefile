all:
	ghc -package llvm-general-pure -dynamic src/bzo.hs src/compiler.hs src/types.hs src/lexer.hs src/tokens.hs src/syntax.hs
	mv src/bzo bin/bzo
clean:
	cd src; rm *.o *.hi

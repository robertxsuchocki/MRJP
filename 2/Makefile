all: bnfc
	happy -gcai ParLatte.y
	alex -g LexLatte.x
	/opt/ghc/7.10.3/bin/ghc --make src/compiler.hs -o compiler

bnfc: src/Latte.cf
	bnfc src/Latte.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm compiler src/compiler.hi src/compiler.o TypeLatte.hi TypeLatte.o

all: bnfc
	happy -gcai ParLatte.y
	alex -g LexLatte.x
	/opt/ghc/7.10.3/bin/ghc --make compiler.hs -o compiler

bnfc: Latte.cf
	bnfc Latte.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm compiler compiler.hi compiler.o

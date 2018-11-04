all: bnfc
	happy -gcai ParInstant.y
	alex -g LexInstant.x
	ghc --make interpreter.hs -o interpreter

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm interpreter interpreter.hi interpreter.o

all: bnfc
	happy -gcai ParInstant.y
	alex -g LexInstant.x
	ghc --make src/interpreter.hs -o interpreter

bnfc: src/Instant.cf
	bnfc src/Instant.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm interpreter src/interpreter.hi src/interpreter.o


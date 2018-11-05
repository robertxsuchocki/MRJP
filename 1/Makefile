all: bnfc
	happy -gcai ParInstant.y
	alex -g LexInstant.x
	ghc --make src/insc_jvm.hs -o insc_jvm
	ghc --make src/insc_llvm.hs -o insc_llvm

bnfc: src/Instant.cf
	bnfc src/Instant.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm insc_jvm src/insc_jvm.hi src/insc_jvm.o
	rm insc_llvm src/insc_llvm.hi src/insc_llvm.o

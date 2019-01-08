all: bnfc
	happy -gcai ParLatte.y
	alex -g LexLatte.x
	ghc --make -isrc src/latc_llvm.hs -o latc_llvm

bnfc: src/Latte.cf
	bnfc src/Latte.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm latc_llvm src/*.hi src/*.o

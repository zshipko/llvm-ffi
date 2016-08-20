test:
	runhaskell Setup configure --user -fbuildExamples
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/llvm-ffi-example/llvm-ffi-example

	make test307
	make test306
	make test305
	make test304

	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 3.4 3.5 3.6 3.7 3.8))

test%:
	runhaskell Setup clean
	runhaskell Setup configure --user -fbuildExamples -fllvm$*
	runhaskell Setup build
	./dist/build/llvm-ffi-example/llvm-ffi-example

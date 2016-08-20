test:
	runhaskell Setup configure --user -fbuildExamples
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/llvm-ffi-example/llvm-ffi-example

	runhaskell Setup clean
	runhaskell Setup configure --user -fbuildExamples -fllvm305
	runhaskell Setup build
	./dist/build/llvm-ffi-example/llvm-ffi-example

	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 3.4 3.5 3.6 3.7 3.8))

test:
	runhaskell Setup configure --user -fbuildExamples -fbuildTools
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/llvm-ffi-example/llvm-ffi-example
	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 3.4 3.5 3.6 3.7 3.8))

test:
	runhaskell Setup configure --user -fbuildTools
	runhaskell Setup build
	runhaskell Setup haddock
	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 3.4 3.5 3.6 3.7 3.8))

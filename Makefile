test:
	runhaskell Setup configure --user -fbuildTools
	runhaskell Setup build
	runhaskell Setup haddock

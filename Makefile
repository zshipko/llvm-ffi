test:
	runhaskell Setup configure --user -fbuildExamples -fpkgConfig
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/llvm-ffi-example/llvm-ffi-example

	make test800
	make test700
	make test600
	make test500
	make test400
	make test309

	make avx-test

# llvm-config-7 --cxxflags lacks the "-std=c++11" option and thus compilation fails
avx-test:
	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 3.9 4.0 5.0 6.0 8 9))

test%:
	runhaskell Setup clean
	runhaskell Setup configure --user -fbuildExamples -fpkgConfig -fllvm$*
	runhaskell Setup build
	./dist/build/llvm-ffi-example/llvm-ffi-example


ctest/fastmath.o:	ctest/fastmath.c
	gcc -Wall -c -o $@ $< -Iinclude/ $$(llvm-config-3.9 --cflags)

ctest/fastmath:	ctest/fastmath.o dist/build/cbits/support.o
	g++ -o $@ $^ $$(llvm-config-3.9 --ldflags) -lLLVM-3.9

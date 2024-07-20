test:
	runhaskell Setup configure --user -fbuildExamples -fpkgConfig
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/llvm-ffi-host/llvm-ffi-host
	./dist/build/llvm-ffi-jit/llvm-ffi-jit

	make test1600
	make test1500
	make test1400
	make test1300

	make avx-test

# llvm-config-7 --cxxflags lacks the "-std=c++11" option and thus compilation fails
# ToDo: for LLVM-17 we need to adapt to new optimizer interface
# ToDo: for LLVM-16 we need to adapt to opaque pointers
avx-test:
	(cd ctest; make $(patsubst %, avx-instruction-selection-%, 15 14 13))

test%:
	runhaskell Setup clean
	runhaskell Setup configure --user -fbuildExamples -fpkgConfig -fllvm$*
	runhaskell Setup build
	./dist/build/llvm-ffi-jit/llvm-ffi-jit


ctest/fastmath.o:	ctest/fastmath.c
	gcc -Wall -c -o $@ $< -Iinclude/ $$(llvm-config-13 --cflags)

ctest/fastmath:	ctest/fastmath.o dist/build/cbits/support.o
	g++ -o $@ $^ $$(llvm-config-13 --ldflags) -lLLVM-13


raw-modules:	$(patsubst %, src/%/LLVM/Raw/Core.hsc, 17 16 15 14 13)

src/%/LLVM/Raw/Core.hsc:	/usr/lib/llvm-%/include/llvm-c/Core.h src/LLVM/Raw/Core_tmpl.hsc
	mkdir -p $$(dirname $@)
	cp src/LLVM/Raw/Core_tmpl.hsc $@
	fgrep -v DumpType $< | llvm-function-mangler >>$@

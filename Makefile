
avx-instruction-selection-%:	ctest/avx-instruction-selection.c
	g++ -Wall -c ctest/create-execution-engine.cpp `llvm-config-$* --cxxflags` -I.
	gcc -Wall -o $@ $< `llvm-config-$* --cflags --ldflags` -I. create-execution-engine.o -lLLVM-$*

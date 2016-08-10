
avx-instruction-selection-%:	ctest/avx-instruction-selection.c ctest/create-execution-engine.cpp
	g++ -Wall -c ctest/create-execution-engine.cpp `llvm-config-$* --cxxflags` -I.
	gcc -Wall -o $@ $< `llvm-config-$* --cflags --ldflags` -I. create-execution-engine.o -lLLVM-$*

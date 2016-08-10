
avx-instruction-selection-%:	avx-instruction-selection.c create-execution-engine.cpp
	g++ -Wall -c create-execution-engine.cpp `llvm-config-$* --cxxflags` -I.
	gcc -Wall -o $@ $< `llvm-config-$* --cflags --ldflags` -I. create-execution-engine.o -lLLVM-$*

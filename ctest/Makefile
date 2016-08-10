
avx-instruction-selection-%:	avx-instruction-selection.c create-execution-engine-%.o
	gcc -Wall -o $@ $< `llvm-config-$* --cflags --ldflags` -I. create-execution-engine-$*.o -lLLVM-$*

create-execution-engine-%.o:	create-execution-engine.cpp
	g++ -Wall -o $@ -c $< `llvm-config-$* --cxxflags` -I.

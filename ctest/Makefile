
avx-instruction-selection-%:	avx-instruction-selection.c create-execution-engine-%.o create-execution-engine.h
	gcc -Wall -o $@ $< $$(llvm-config-$* --cflags --ldflags) -I. create-execution-engine-$*.o -lLLVM-$* -lstdc++

create-execution-engine-%.o:	create-execution-engine.cpp create-execution-engine.h
	g++ -Wall -o $@ -c $< $$(llvm-config-$* --cxxflags) -I.

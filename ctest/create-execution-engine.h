#include "llvm-c/ExecutionEngine.h"

#ifdef __cplusplus
extern "C" {
#endif

LLVMBool LLVMCreateExecutionEngineForModuleCPU
    (LLVMExecutionEngineRef *OutEE,
     LLVMModuleRef M,
     char **OutError);

void LLVMSetHasNoSignedZeros (LLVMValueRef Instr, LLVMBool B);

#ifdef __cplusplus
} /* extern "C" */
#endif

#ifndef LLVM_HS_SUPPORT_H
#define LLVM_HS_SUPPORT_H

#ifdef __cplusplus
extern "C" {
#endif

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
				      unsigned OptimizationLevel);

void LLVMCreateStandardModulePasses(LLVMPassManagerRef PM,
				    unsigned OptimizationLevel,
				    int OptimizeSize,
				    int UnitAtATime,
				    int UnrollLoops,
				    int SimplifyLibCalls,
				    int HaveExceptions,
				    int DisableInlining);

const char *LLVMGetHostCPUName(size_t &len);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LLVM_HS_SUPPORT_H */

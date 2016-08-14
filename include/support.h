#ifndef LLVM_HS_SUPPORT_H
#define LLVM_HS_SUPPORT_H


#ifdef __cplusplus
typedef llvm::StringMap<bool> LLVMFeatureMap;
typedef llvm::StringMap<bool>::const_iterator LLVMFeatureIterator;

extern "C" {
#else
typedef int LLVMFeatureMap;
typedef int LLVMFeatureIterator;
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


typedef LLVMFeatureMap *LLVMFeatureMapRef;
typedef LLVMFeatureIterator *LLVMFeatureIteratorRef;

LLVMFeatureMapRef LLVMGetHostFeatures();
void LLVMFreeFeatures(LLVMFeatureMapRef features);

LLVMFeatureIteratorRef LLVMGetFirstFeature(LLVMFeatureMapRef features);
LLVMFeatureIteratorRef LLVMGetNextFeature(LLVMFeatureMapRef features, LLVMFeatureIteratorRef featureRef);

const char *LLVMGetFeatureName(LLVMFeatureIteratorRef featureRef);
LLVMBool LLVMGetFeatureSupport(LLVMFeatureIteratorRef featureRef);



LLVMBool LLVMCreateExecutionEngineForModuleCPU
  (LLVMExecutionEngineRef *OutEE,
   LLVMModuleRef M,
   char **OutError);


void LLVMSetHasUnsafeAlgebra(LLVMValueRef Instr, LLVMBool B);
void LLVMSetHasNoNaNs(LLVMValueRef Instr, LLVMBool B);
void LLVMSetHasNoInfs(LLVMValueRef Instr, LLVMBool B);
void LLVMSetHasNoSignedZeros(LLVMValueRef Instr, LLVMBool B);
void LLVMSetHasAllowReciprocal(LLVMValueRef Instr, LLVMBool B);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LLVM_HS_SUPPORT_H */

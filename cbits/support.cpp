#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "llvm-c/Core.h"
#include "llvm/PassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"

#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Host.h"

#include "support.h"

using namespace llvm;

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
					unsigned OptimizationLevel)
{
#if HS_LLVM_VERSION >= 300
  llvm::PassManagerBuilder Builder;
  Builder.OptLevel = OptimizationLevel;

  llvm::PassManagerBase *pass_man = unwrap(PM);
  llvm::FunctionPassManager *func_man =
    static_cast <FunctionPassManager*>(pass_man);

  if (func_man) {
    Builder.populateFunctionPassManager (*func_man);
  } else {
    // printf ("Cannot create function passes for module pass manager\n");
  }
#else
  createStandardFunctionPasses(unwrap(PM), OptimizationLevel);
#endif
}

void LLVMCreateStandardModulePasses(LLVMPassManagerRef PM,
				    unsigned OptLevel,
				    int OptimizeSize,
				    int UnitAtATime,
				    int UnrollLoops,
				    int SimplifyLibCalls,
				    int HaveExceptions,
				    int DisableInline)
{
#if HS_LLVM_VERSION >= 300
  llvm::PassManagerBuilder Builder;
  Builder.OptLevel = OptLevel;
  Builder.SizeLevel = OptimizeSize;
  Builder.DisableUnrollLoops = !UnrollLoops;
#if HS_LLVM_VERSION < 304
  Builder.DisableSimplifyLibCalls = !SimplifyLibCalls;
#endif
  Builder.DisableUnitAtATime = !UnitAtATime;

  Pass *InliningPass = 0;

  if (DisableInline) {
    // No inlining pass
  } else if (OptLevel) {
    unsigned Threshold = 225;
    if (OptLevel > 2)
      Threshold = 275;
    Builder.Inliner = createFunctionInliningPass(Threshold);
  } else {
    Builder.Inliner = createAlwaysInlinerPass();
  }

  Builder.populateModulePassManager (*unwrap(PM));
#else
  Pass *InliningPass = 0;

  if (DisableInline) {
    // No inlining pass
  } else if (OptLevel) {
    unsigned Threshold = 225;
    if (OptLevel > 2)
      Threshold = 275;
    InliningPass = createFunctionInliningPass(Threshold);
  } else {
    InliningPass = createAlwaysInlinerPass();
  }

  createStandardModulePasses(unwrap(PM), OptLevel, OptimizeSize,
                             UnitAtATime, UnrollLoops, SimplifyLibCalls,
                             HaveExceptions, InliningPass);
#endif
}

const char *LLVMGetHostCPUName(size_t &len) {
  StringRef r = sys::getHostCPUName();
  len = r.size();
  return r.data();
}



/*
getHostCPUFeatures supports X86 only starting with LLVM-3.7
How does llc -mattr=help work? It seems to emit directly to stderr.
*/
LLVMFeatureMapRef LLVMGetHostFeatures() {
  LLVMFeatureMapRef features = new(LLVMFeatureMap);
  if (sys::getHostCPUFeatures(*features)) {
    return features;
  } else {
    delete features;
    return nullptr;
  }
}

void LLVMFreeFeatures(LLVMFeatureMapRef features) {
  delete(features);
}

LLVMFeatureIteratorRef LLVMCheckFeature
    (LLVMFeatureMapRef features, LLVMFeatureIteratorRef featureRef) {
  if (*featureRef == features->end()) {
    delete featureRef;
    return nullptr;
  } else {
    return featureRef;
  }
}

LLVMFeatureIteratorRef LLVMGetFirstFeature(LLVMFeatureMapRef features) {
  return LLVMCheckFeature(features, new LLVMFeatureIterator(features->begin()));
}

LLVMFeatureIteratorRef LLVMGetNextFeature
    (LLVMFeatureMapRef features, LLVMFeatureIteratorRef featureRef) {
  (*featureRef)++;
  return LLVMCheckFeature(features, featureRef);
}

const char *LLVMGetFeatureName(LLVMFeatureIteratorRef featureRef) {
  return (*featureRef)->first().data();
}

LLVMBool LLVMGetFeatureSupport(LLVMFeatureIteratorRef featureRef) {
  return (*featureRef)->second;
}



LLVMBool LLVMCreateExecutionEngineForModuleCPU
  (LLVMExecutionEngineRef *OutEE,
   LLVMModuleRef M,
   char **OutError) {
  std::string Error;
#if HS_LLVM_VERSION < 306
  EngineBuilder builder(unwrap(M));
#else
  EngineBuilder builder(std::unique_ptr<Module>(unwrap(M)));
#endif
  builder.setEngineKind(EngineKind::Either)
         .setMCPU(sys::getHostCPUName().data())
         .setErrorStr(&Error);
  if (ExecutionEngine *EE = builder.create()){
    *OutEE = wrap(EE);
    return 0;
  }
  *OutError = strdup(Error.c_str());
  return 1;
}


void LLVMSetHasUnsafeAlgebra(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasUnsafeAlgebra(B);
}
void LLVMSetHasNoNaNs(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoNaNs(B);
}
void LLVMSetHasNoInfs(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoInfs(B);
}
void LLVMSetHasNoSignedZeros(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasNoSignedZeros(B);
}
void LLVMSetHasAllowReciprocal(LLVMValueRef Instr, LLVMBool B) {
  (unwrap<Instruction>(Instr))->setHasAllowReciprocal(B);
}

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

typedef StringMap<bool> LLVMFeatureMap;
typedef LLVMFeatureMap *LLVMFeatureMapRef;

LLVMFeatureMapRef LLVMGetHostFeatures() {
  LLVMFeatureMapRef features = new(LLVMFeatureMap);
  if (sys::getHostCPUFeatures(*features)) {
    return features;
  } else {
    return nullptr;
  }
}

void LLVMFreeFeatures(LLVMFeatureMapRef features) {
  delete(features);
}

typedef llvm::StringMap<bool>::const_iterator LLVMFeatureIterator;
struct LLVMFeature
  {LLVMFeature (LLVMFeatureMapRef map0, LLVMFeatureIterator it0)
     : map(map0), it(it0) {};
   LLVMFeatureMapRef map; LLVMFeatureIterator it; };
typedef LLVMFeature *LLVMFeatureRef;

LLVMFeatureRef LLVMCheckFeature(LLVMFeatureRef featureRef) {
  if (featureRef->it == featureRef->map->end()) {
    return featureRef;
  } else {
    delete featureRef;
    return nullptr;
  }
}

LLVMFeatureRef LLVMGetFirstFeature(LLVMFeatureMapRef features) {
  return LLVMCheckFeature(new LLVMFeature(features, features->begin()));
}

LLVMFeatureRef LLVMGetNextFeature(LLVMFeatureRef featureRef) {
  (featureRef->it)++;
  return LLVMCheckFeature(featureRef);
}

const char *LLVMGetFeatureName(LLVMFeatureRef featureRef) {
  return featureRef->it->first().data();
}

LLVMBool LLVMGetFeatureSupport(LLVMFeatureRef featureRef) {
  return featureRef->it->second;
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

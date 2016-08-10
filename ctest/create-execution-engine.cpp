#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Host.h"

#include "create-execution-engine.h"

using namespace llvm;

#define LLVM_VERSION (LLVM_VERSION_MAJOR * 100 + LLVM_VERSION_MINOR)

LLVMBool LLVMCreateExecutionEngineForModuleCPU
    (LLVMExecutionEngineRef *OutEE,
     LLVMModuleRef M,
     char **OutError) {
  std::string Error;
#if LLVM_VERSION < 306
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

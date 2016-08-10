#include <llvm/Config/config.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/Scalar.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "create-execution-engine.h"

#if 0
const int vectorSize = 4;
const char* roundName = "llvm.x86.sse41.round.ps";
#else
const int vectorSize = 8;
const char* roundName = "llvm.x86.avx.round.ps.256";
#endif

int main ()
{
  LLVMModuleRef module;
  LLVMExecutionEngineRef execEngine;
  LLVMTargetDataRef targetData;
  LLVMTypeRef vectorType, ptrType, voidType, funcType, roundType, int32Type;
  LLVMValueRef func, roundFunc;
  LLVMValueRef param, loaded, const1, callRound;
  LLVMBuilderRef builder;
  LLVMBasicBlockRef block;
  const int false = 0;

#if 0
  // this is the expansion of LLVMInitializeNativeTarget
  LLVMInitializeX86TargetInfo();
  LLVMInitializeX86Target();
  LLVMInitializeX86TargetMC();
#else
  LLVMInitializeNativeTarget();
#endif
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();
  module = LLVMModuleCreateWithName("_module");
  LLVMSetTarget(module, LLVM_HOST_TRIPLE);
#if 1
  vectorType = LLVMVectorType(LLVMFloatType(), vectorSize);
#else
  vectorType = LLVMFloatType();
#endif
  ptrType = LLVMPointerType(vectorType, 0);
  voidType = LLVMVoidType();
  LLVMTypeRef roundParams[] = { ptrType };
  roundType = LLVMFunctionType(voidType, roundParams, 1, false);
  func = LLVMAddFunction(module, "round", roundType);
  LLVMSetLinkage(func, LLVMExternalLinkage);
  builder = LLVMCreateBuilder();
  block = LLVMAppendBasicBlock(func, "_L1");
  LLVMPositionBuilderAtEnd(builder, block);
  param = LLVMGetParam(func, 0);
  loaded = LLVMBuildLoad(builder, param, "");
  int32Type = LLVMIntType(32);
  LLVMTypeRef funcParams[] = { vectorType, int32Type } ;
  funcType = LLVMFunctionType(vectorType, funcParams, 2, false);
  roundFunc = LLVMAddFunction(module, roundName, funcType);
  LLVMSetLinkage(roundFunc, LLVMExternalLinkage);
  const1 = LLVMConstInt(int32Type, 1, false);
  LLVMValueRef callParams [] = { loaded, const1 } ;
  callRound = LLVMBuildCall(builder, roundFunc, callParams, 2, "");
  LLVMSetInstructionCallConv(callRound, LLVMCCallConv);
  LLVMAddInstrAttribute(callRound, 0, 0);
  LLVMBuildStore(builder, callRound, param);
  LLVMBuildRetVoid(builder);
  LLVMWriteBitcodeToFile(module, "round-avx.bc");
  char *errorMsg;
  LLVMCreateExecutionEngineForModuleCPU(&execEngine, module, &errorMsg);
  if (!execEngine) {
    printf ("LLVMCreateExecutionEngine: %s\n", errorMsg);
    return 1;
  }
  targetData = LLVMGetExecutionEngineTargetData(execEngine);
  size_t vectorSize0 = LLVMStoreSizeOfType(targetData, vectorType);
  size_t vectorAlign = LLVMABIAlignmentOfType(targetData, vectorType);
  float vector[vectorSize] __attribute__((aligned(32)));
  printf("%lx, size %lx, align %lx\n", (size_t)vector, vectorSize0, vectorAlign);
  LLVMGenericValueRef runParams[] =
    { LLVMCreateGenericValueOfPointer(vector) } ;
  LLVMRunFunction(execEngine, func, 1, runParams);
  return 0;
}

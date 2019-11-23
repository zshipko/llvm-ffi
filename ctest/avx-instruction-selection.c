#include <llvm/Config/llvm-config.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>

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
  LLVMValueRef param, loaded, callRound;
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
  int32Type = LLVMInt32Type();
  LLVMTypeRef funcParams[] = { vectorType, int32Type } ;
  funcType = LLVMFunctionType(vectorType, funcParams, 2, false);
  roundFunc = LLVMAddFunction(module, roundName, funcType);
  LLVMSetLinkage(roundFunc, LLVMExternalLinkage);
#if 1
  LLVMValueRef const1 = LLVMConstInt(int32Type, 1, false);
  LLVMValueRef callParams [] = { loaded, const1 } ;
  callRound = LLVMBuildCall(builder, roundFunc, callParams, 2, "");
  LLVMSetInstructionCallConv(callRound, LLVMCCallConv);
#else
  const int true = 1;
  LLVMBuildFAdd(builder, loaded, loaded, "");
  LLVMValueRef zero = LLVMConstNull (vectorType);
  callRound = LLVMBuildFAdd(builder, loaded, zero, "");
  LLVMSetHasNoSignedZeros(callRound, true);
#endif
  LLVMBuildStore(builder, callRound, param);
  LLVMBuildRetVoid(builder);
  LLVMWriteBitcodeToFile(module, "round-avx.bc");

#if 1
  LLVMPassManagerRef mpasses = LLVMCreatePassManager();
  LLVMPassManagerRef fpasses = LLVMCreatePassManager();
  LLVMAddVerifierPass(mpasses);

  LLVMPassManagerBuilderRef fpassBuilder = LLVMPassManagerBuilderCreate();
  LLVMPassManagerBuilderSetOptLevel(fpassBuilder, 3);
  LLVMPassManagerBuilderPopulateFunctionPassManager(fpassBuilder, fpasses);
  LLVMPassManagerBuilderDispose (fpassBuilder);

  LLVMPassManagerBuilderRef mpassBuilder = LLVMPassManagerBuilderCreate();
  LLVMPassManagerBuilderSetOptLevel(mpassBuilder, 3);
  LLVMPassManagerBuilderPopulateModulePassManager(mpassBuilder, mpasses);
  LLVMPassManagerBuilderDispose (mpassBuilder);

  LLVMRunPassManager(fpasses, module);
  LLVMRunPassManager(mpasses, module);
  LLVMDisposePassManager(fpasses);
  LLVMDisposePassManager(mpasses);
  LLVMWriteBitcodeToFile(module, "round-avx-opt.bc");
#endif

  char *errorMsg;
#if 0
  {
    LLVMMemoryBufferRef mem;
    if (LLVMCreateMemoryBufferWithContentsOfFile
          ("round-avx.bc", &mem, &errorMsg)) {
      printf ("LLVMCreateMemoryBufferWithContentsOfFile: %s\n", errorMsg);
      return 1;
    }
    if (LLVMParseBitcode(mem, &module, &errorMsg)) {
      printf ("LLVMParseBitcode: %s\n", errorMsg);
      return 1;
    }
    func = LLVMGetNamedFunction(module, "round");
    LLVMDisposeMemoryBuffer (mem);
  }
#endif

  if (LLVMCreateExecutionEngineForModuleCPU(&execEngine, module, &errorMsg)) {
    printf ("LLVMCreateExecutionEngine: %s\n", errorMsg);
    return 1;
  }
  targetData = LLVMGetExecutionEngineTargetData(execEngine);
  size_t vectorSize0 = LLVMStoreSizeOfType(targetData, vectorType);
  size_t vectorAlign = LLVMABIAlignmentOfType(targetData, vectorType);
  float vector[vectorSize] __attribute__((aligned(32)));
  printf("%lx, size %lx, align %lx\n",
    (size_t)vector, vectorSize0, vectorAlign);

  {
    float x = -1.3;
    int k;
    for (k = 0; k<vectorSize; k++) { vector[k] = x; x += 1; }
  }
#if 0
  LLVMGenericValueRef runParams[] =
    { LLVMCreateGenericValueOfPointer(vector) } ;
  LLVMRunFunction(execEngine, func, 1, runParams);
#else
  void (*funcPtr) (float *);
  funcPtr = LLVMGetPointerToGlobal(execEngine, func);
  funcPtr(vector);
#endif
  {
    int k;
    printf("vector:");
    for (k = 0; k<vectorSize; k++) { printf(" %f", vector[k]); }
    printf("\n");
  }
  LLVMDisposeExecutionEngine(execEngine);
  return 0;
}

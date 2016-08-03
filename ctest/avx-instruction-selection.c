#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/Scalar.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
  LLVMTypeRef floatType, vectorType, ptrType, voidType, funcType, roundType, int32Type;
  LLVMValueRef func, roundFunc;
  LLVMValueRef param, loaded, const1, callRound;
  LLVMBuilderRef builder;
  LLVMBasicBlockRef block;
  const int false = 0;

  LLVMInitializeX86TargetInfo();
  LLVMInitializeX86Target();
  LLVMInitializeX86TargetMC();
  module = LLVMModuleCreateWithName("_module");
  LLVMSetTarget(module, "x86_64-unknown-linux-gnu");
  floatType = LLVMFloatType();
  vectorType = LLVMVectorType(floatType, vectorSize);
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
  LLVMSetInstructionCallConv(callRound, 0);
  LLVMAddInstrAttribute(callRound, 0, 0);
  LLVMBuildStore(builder, callRound, param);
  LLVMBuildRetVoid(builder);
  LLVMWriteBitcodeToFile(module, "round-avx.bc");
  char *errorMsg;
  LLVMCreateExecutionEngineForModule(&execEngine, module, &errorMsg);
  targetData = LLVMGetExecutionEngineTargetData(execEngine);
  size_t vectorSize0 = LLVMStoreSizeOfType(targetData, vectorType);
  size_t vectorAlign = LLVMABIAlignmentOfType(targetData, vectorType);
  float vector[vectorSize];
  printf("%lx, size %lx, align %lx\n", (size_t)vector, vectorSize0, vectorAlign);
  LLVMGenericValueRef genericVector = LLVMCreateGenericValueOfPointer(vector);
  LLVMGenericValueRef runParams[] = { genericVector } ;
  LLVMRunFunction(execEngine, func, 1, runParams);
  return 0;
}

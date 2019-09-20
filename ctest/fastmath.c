#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/Scalar.h>
#include "support.h"
#include <stdio.h>

int main ()
{
  LLVMInitializeX86TargetInfo ();
  LLVMInitializeX86Target ();
  LLVMModuleRef module = LLVMModuleCreateWithName ("_module");
  LLVMTypeRef type_double = LLVMDoubleType ();
  LLVMTypeRef double_pair[] = { type_double, type_double };

  LLVMValueRef func = LLVMAddFunction (module, "arithmetic",
                                       LLVMFunctionType (type_double,
                                                         double_pair, 2, 0));
  LLVMSetLinkage (func, LLVMExternalLinkage);
  LLVMBuilderRef builder = LLVMCreateBuilder ();
  LLVMPositionBuilderAtEnd (builder, LLVMAppendBasicBlock (func, "_L1"));

  LLVMTypeRef type_func = LLVMFunctionType (type_double, &type_double, 1, 0);
  LLVMValueRef func_exp = LLVMAddFunction (module, "llvm.exp.f64", type_func);
  LLVMSetLinkage (func_exp, 0);
  LLVMValueRef param0 = LLVMGetParam (func, 0);
  LLVMValueRef call_exp = LLVMBuildCall (builder, func_exp, &param0, 1, "");
  LLVMSetHasNoSignedZeros(call_exp, 1);
  LLVMSetInstructionCallConv (call_exp, 0);
  LLVMValueRef func_sin = LLVMAddFunction (module, "llvm.sin.f64", type_func);
  LLVMSetLinkage (func_sin, 0);
  LLVMValueRef param1 = LLVMGetParam (func, 1);
  LLVMValueRef call_sin = LLVMBuildCall (builder, func_sin, &param1, 1, "");
  LLVMSetHasNoNaNs(call_sin, 1);
  LLVMSetInstructionCallConv (call_sin, 0);
  LLVMValueRef sum = LLVMBuildFAdd (builder, call_exp, call_sin, "");
  LLVMSetHasUnsafeAlgebra(sum, 1);
  LLVMBuildRet (builder, sum);
  LLVMWriteBitcodeToFile (module, "fast-math.bc");
  LLVMPassManagerRef pm = LLVMCreatePassManager ();
  LLVMAddVerifierPass (pm);
  LLVMRunPassManager (pm, module);
  LLVMWriteBitcodeToFile (module, "fast-math-verified.bc");
  LLVMDisposeBuilder (builder);
  return 0;
}

/*
 * Copyright (c) 2008-10, Mahadevan R All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of this software, nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * These are some "extra" functions not available in the standard LLVM-C
 * bindings, but are required / good-to-have inorder to implement the
 * Python bindings.
 */

#ifndef LLVM_PY_EXTRA_H
#define LLVM_PY_EXTRA_H

#ifdef __cplusplus
extern "C" {
#endif

/* Notes:
 *  - Some returned strings must be disposed of by LLVMDisposeMessage. These are
 *    indicated in the comments. Where it is not indicated, DO NOT call dispose.
 */


/* Wraps the LLVMInitializeTarget macro from Target.h */
unsigned LLVMInitNativeTarget(void);

/* Wraps llvm::Value::getNumUses(). */
unsigned LLVMValueGetNumUses(LLVMValueRef value);

/* Wraps llvm::Instruction::getOpcode(). */
unsigned LLVMInstGetOpcode(LLVMValueRef inst);

/* Wraps llvm::CmpInst::getPredicate(). */
unsigned LLVMCmpInstGetPredicate(LLVMValueRef cmpinst);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LLVM_PY_EXTRA_H */


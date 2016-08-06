#ifdef __cplusplus
extern "C" {
#endif

LLVMBool LLVMCreateExecutionEngineForModuleCPU
    (LLVMExecutionEngineRef *OutEE,
     LLVMModuleRef M,
     char **OutError);

#ifdef __cplusplus
} /* extern "C" */
#endif

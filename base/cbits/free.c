#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>

/* C function to free function object resources.  Can be called from a finalizer. */
void
c_freeFunctionObject(LLVMExecutionEngineRef execEngine,
		     LLVMValueRef f)
{
  LLVMFreeMachineCodeForFunction(execEngine, f);
}

void
c_freeModuleProvider(LLVMExecutionEngineRef execEngine,
		     LLVMModuleProviderRef moduleProvider)
{
  LLVMModuleRef mod;
  if (!LLVMRemoveModuleProvider(execEngine, moduleProvider, &mod, 0)) {
    LLVMDisposeModule(mod);
  }
}

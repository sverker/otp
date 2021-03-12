#include <asmjit/x86.h>
#include <stdio.h>

using namespace asmjit;

// Signature of the generated function.
typedef int (*Func)(int rdi, int rsi);

#define CHK(ERR) chk(ERR, __LINE__)

static void chk(Error err, int line)
{
    if (err) {
        fprintf(stderr, "ERROR:%d: %s\n", line, DebugUtils::errorAsString(err));
        abort();
    }
}

static int mul(int a, int b)
{
    printf("a=%d b=%d\n", a, b);
}

int main() {
  JitRuntime rt;                    // Runtime specialized for JIT code execution.

  CodeHolder code;                  // Holds code and relocation information.
  code.init(rt.environment());      // Initialize code to match the JIT environment

  x86::Assembler a(&code);          // Create and attach x86::Assembler to code.

  Label again = a.newLabel();

  a.addValidationOptions(BaseEmitter::kValidationOptionAssembler);

  a.bind(again);
  CHK(a.push(x86::rdi));
  CHK(a.push(x86::rsi));
  CHK(a.call(Imm(&mul)));
  CHK(a.pop(x86::rsi));
  CHK(a.pop(x86::rdi));
  CHK(a.dec(x86::edi));
  CHK(a.jnz(again));

  a.ret();                          // Return from function.
  // ===== x86::Assembler is no longer needed from here and can be destroyed =====

  Func fn;                          // Holds address to the generated function.
  Error err = rt.add(&fn, &code);   // Add the generated code to the runtime.
  if (err) return 1;                // Handle a possible error returned by AsmJit.
  // ===== CodeHolder is no longer needed from here and can be destroyed =====

  int result = fn(7,8);             // Execute the generated code.
  printf("%d\n", result);           // Print the resulting "1".

  // All classes use RAII, all resources will be released before `main()` returns,
  // the generated function can be, however, released explicitly if you intend to
  // reuse or keep the runtime alive, which you should in a production-ready code.
  rt.release(fn);

  return 0;
}

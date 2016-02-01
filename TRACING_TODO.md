* Implement tracing nif backends
 - code purging of backends
 - test cases (especially for nif tracer backend)
 - documentation
* Rewrite process + port tracing to use tracing nif backends (done)
* Write lttng/dtrace/systemtap backends (maybe put backends in dyntrace?)
* Go through ttb and dbg documentation, add dbg User's Guide.
  - Especially add examples of complex match spec uses (eg. turning on and off tracing)
* Teach dbg to do wildcard module matching based on strings, i.e. dbg("mnesia.*",...)
* Implement load shedding mechanisms for proc/port tracers
* Implement sampling profiler backend
* Implement `erlang:trace_pattern(send, true, [{{'$1','$2'},[{'/=',{node},{node,'$2'}}],[]}]).`
 - Check if we want to do this for any other trace flags as well? `receive`? `link`?
 - Maybe implement a way to do match spec filtering on return_trace events?
   - `erlang:trace_pattern({lists,keyfind,'_'},true,[{'_',[],[{return_trace,[{['$1'],[{'==','$1',{const,false}}],[]}]}]}]).`
     problem with this approach is that variables ('$1') are defined in two scopes....
* Call trace backends in Erlang not C
* Implement trace sessions (i.e. multiple tracers)

* Implement tracing nif backends
 - code purging of backends
 - test cases (especially for nif tracer backend)
  - unload/reload tracer module
  - remove return from enabled in match spec
  - SOL + SOL1 + SCHED_EXIT + port trace
  - trace_info(on_load, meta | meta_match_spec | all | call_time | call_count)
  - erts_port_output_async to outputv (i.e. file tracing)
  - new nif apis
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

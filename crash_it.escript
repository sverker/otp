#!/usr/bin/env escript

%% The following enables memory tracing in the emulator:
%%! +Mim true

%% Run as:
%% rm -f crash.beam && ./crash_it.escript [timeout_seconds]

main(Args) ->
    make:all([native,{hipe,[verbose,o3]}]), % trips the bug
    %%make:all([]), % does not trip the bug

    process_args(Args),
    io:fwrite("OTP ~s.~n", [erlang:system_info(otp_release)]),

    %% Sometimes a process won't trip the bug, regardless how many iterations,
    %% so we start a number of parallel processes to increase the probability
    %% that one of them trips the bug.
    %%    The bug seems to be related to a minor GC not yet executed. The GC
    %% settings below seem to increase the rate of tripping the bug.
    Instances = 10,
    lists:foreach(
      fun (Instance) ->
              erlang:spawn_opt(crash, start, [Instance],
                               [link,
                                {min_heap_size,1},
                                {min_bin_vheap_size,1},
                                {fullsweep_after,1000000000}])
      end,
      lists:seq(1, Instances)),
    receive never -> ok end.

process_args([]) ->
    ok;
process_args([TimeoutStr]) ->
    Timeout = list_to_integer(TimeoutStr) * 1000, % [ms]
    spawn_link(fun() -> halt_after(Timeout) end),
    ok;
process_args(_) ->
    io:fwrite(standard_error,
              "usage: crash_it.escript [timeout_seconds]~n", []),
    erlang:halt(2).

halt_after(Timeout) ->
    timer:sleep(Timeout),
    Status = 3,
    io:fwrite("bug not tripped after ~w ms, exit with status ~w~n",
              [Timeout, Status]),
    erlang:halt(Status).

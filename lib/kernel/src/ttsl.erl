-module(ttsl).

-export([init/0,
         start/1, putc/1, move_rel/1, insc/1, delc/1, beep/0,
         get_window_size/0,
         get_unicode_state/0,
         set_unicode_state/1,
         continue_write/0,
         read/0]).

init() ->
    ok = erlang:load_nif(atom_to_list(?MODULE), 0).

start(_Cmd) -> erlang:nif_error(undefined).
putc(_Bin) -> erlang:nif_error(undefined).
move_rel(_N) -> erlang:nif_error(undefined).
insc(_Bin) -> erlang:nif_error(undefined).
delc(_N) -> erlang:nif_error(undefined).
beep() -> erlang:nif_error(undefined).
get_window_size() -> erlang:nif_error(undefined).
get_unicode_state() -> erlang:nif_error(undefined).
set_unicode_state(_) -> erlang:nif_error(undefined).
continue_write() -> erlang:nif_error(undefined).
read() -> erlang:nif_error(undefined).

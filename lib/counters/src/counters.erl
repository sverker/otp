%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% Purpose : Main counters API module.

-module(counters).

-export([new/2, add/3, add_get/3, sub/3, sub_get/3, put/3, get/2]).

-export_type([counters_ref/0]).

-opaque counters_ref() :: reference().

-compile(no_native).
-on_load(on_load/0).
-define(COUNTERS_NIF_VSN,101).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).


on_load() ->
    LibBaseName = "counters",
    PrivDir = code:priv_dir(counters),
    LibName = case erlang:system_info(build_type) of
		  opt ->
		      LibBaseName;
		  Type ->
		      LibTypeName = LibBaseName ++ "."  ++ atom_to_list(Type),
		      case (filelib:wildcard(
			      filename:join(
				[PrivDir,
				 "lib",
				 LibTypeName ++ "*"])) /= []) orelse
			  (filelib:wildcard(
			     filename:join(
			       [PrivDir,
				"lib",
				erlang:system_info(system_architecture),
				LibTypeName ++ "*"])) /= []) of
			  true -> LibTypeName;
			  false -> LibBaseName
		      end
	      end,
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, ?COUNTERS_NIF_VSN) of
		 ok -> ok;
		 {error, {load_failed, _}}=Error1 ->
		     ArchLibDir =
			 filename:join([PrivDir, "lib",
					erlang:system_info(system_architecture)]),
		     Candidate =
			 filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
		     case Candidate of
			 [] -> Error1;
			 _ ->
			     ArchLib = filename:join([ArchLibDir, LibName]),
			     erlang:load_nif(ArchLib, ?COUNTERS_NIF_VSN)
		     end;
		 Error1 -> Error1
	     end,
    case Status of
	ok -> ok;
	{error, {E, Str}} ->
	    error_logger:error_msg("Unable to load counters library. "
                                   "Failed with error:~n\"~p, ~s\"~n", [E,Str]),
	    Status
    end.

-spec new(Arity, Opts) -> counters_ref() when
      Arity :: pos_integer(),
      Opts :: [].
new(_Arity, _Opts) ->
    ?nif_stub.

-spec put(Ref, Ix, Value) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Value :: integer().
put(_Ref, _Ix, _Value) ->
    ?nif_stub.

-spec get(Ref, Ix) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer().
get(_Ref, _Ix) ->
    ?nif_stub.

-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Incr :: integer().
add(_Ref, _Ix, _Incr) ->
    ?nif_stub.

-spec add_get(Ref, Ix, Incr) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Incr :: integer().
add_get(_Ref, _Ix, _Incr) ->
    ?nif_stub.

-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    add(Ref, Ix, -Decr).

-spec sub_get(Ref, Ix, Decr) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Decr :: integer().
sub_get(Ref, Ix, Decr) ->
    add_get(Ref, Ix, -Decr).


%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of Fanterlastic Four.
%%
%% Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking Fanterlastic Four statically or dynamically with other modules is making a combined work based on Fanterlastic Four. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of Fanterlastic Four give you permission to combine Fanterlastic Four program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Fanterlastic Four and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%% @doc This is a daytime server implemented using gen_daytime of Serlvers (http://software.pupeno.com/Serlvers).
%% @see echo
%% @see chargen
%% @see time
%% @since 0.0.0

-module(time).
-behaviour(gen_time).
-export([start/0, start/1, start_link/0, start_link/1, stop/1]).
-export([init/1, time/1, terminate/2]).

%% @doc Start an unnamed daytime server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start() ->
    %%io:fwrite("~w:start()~n", [?MODULE]),
    gen_time:start(?MODULE, [], []).

%% @doc Start a named daytime server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(SupName) ->
    %%io:fwrite("~w:start(~w)~n", [?MODULE, SupName]),
    gen_time:start(SupName, ?MODULE, [], []).

%% @doc Start an unnamed daytime server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link() ->
    %%io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_time:start_link(?MODULE, [], []).

%% @doc Start a named daytime server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(SupName) ->
    %%io:fwrite("~w:start_link(~w)~n", [?MODULE, SupName]),
    gen_time:start_link(SupName, ?MODULE, [], []).

%% @doc Stop a process.
%% @see start/0
%% @see start/1
%% @see start_link/0
%% @see start_link/1
%% @since 0.0.0
%% @spec (Name) -> ok
%%   Name = atom() | {local, atom()} | {global, atom()}
stop(Process) ->
    %io:fwrite("~w:stop(~w)~n", [?MODULE, Process]),
    gen_time:stop(Process).

%% @doc Initialize the daytime server... nothing really.
%% @private Only gen_daytime should call this function.
%% @since 0.0.0
init(_Args) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, []}.
    
%% @doc The main function, generates and returns the daytime.
%% @private Only gen_daytime should call this function.
%% @since 0.0.0 
time(State) ->
    %%io:fwrite("~w:time(~w)~n", [?MODULE, State]),
    Epoch1900 = {{1900, 01, 01}, {0, 0, 0}},
    TimeSinceEpoch = calendar:time_difference(Epoch1900, calendar:universal_time()),
    TimeInSeconds = timeToSeconds(TimeSinceEpoch),
    SecondsAsString = lists:flatten(io_lib:format("~w", [TimeInSeconds])),
    {SecondsAsString, State}.

%% @doc Clean up.
%% @private Only gen_daytime should call this function.
%% @since 0.0.0
terminate(_Reason, _State) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, _State]),
    ok.

%% @doc Turn a time (days, hours, minutes, seconds) into seconds.
%% @private Internal helper function.
%% @since 0.0.0
timeToSeconds({Days, {Hours, Minutes, Seconds}}) ->
    %%io:fwrite("~w:terminate(~w)~n", [?MODULE, {Days, {Hours, Minutes, Seconds}}]),
    ((Days * 24 + Hours) * 60 + Minutes) * 60 + Seconds.

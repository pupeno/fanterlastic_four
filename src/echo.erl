%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of FanterlasticFour.
%%
%% FanterlasticFour is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% FanterlasticFour is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with FanterlasticFour; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking FanterlasticFour statically or dynamically with other modules is making a combined work based on FanterlasticFour. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of FanterlasticFour give you permission to combine FanterlasticFour program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for FanterlasticFour and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%% @doc This is a echo server implemented using gen_echo of Serlvers (http://software.pupeno.com/Serlvers).
%% @see chargen
%% @see daytime
%% @see time
%% @since 0.0.0

-module(echo).
-behaviour(gen_echo).
-export([start/0, start/1, start_link/0, start_link/1, stop/1]).
-export([init/1, echo/2, terminate/2, transports/0]).

%% @doc Start an unnamed echo server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start() ->
    %%io:fwrite("~w:start()~n", [?MODULE]),
    gen_echo:start(?MODULE, [], []).

%% @doc Start a named echo server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(SupName) ->
    %%io:fwrite("~w:start(~w)~n", [?MODULE, SupName]),
    gen_echo:start(SupName, ?MODULE, [], []).

%% @doc Start an unnamed echo server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link() ->
    %%io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_echo:start_link(?MODULE, [], []).

%% @doc Start a named echo server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(SupName) ->
    %%io:fwrite("~w:start_link(~w)~n", [?MODULE, SupName]),
    gen_echo:start_link(SupName, ?MODULE, [], []).

%% @doc Stop a process.
%% @see start/0
%% @see start/1
%% @see start_link/0
%% @see start_link/1
%% @since 0.0.0
%% @spec (Name) -> ok
%%   Name = atom() | {local, atom()} | {global, atom()}
stop(Process) ->
    %%io:fwrite("~w:stop(~w)~n", [?MODULE, Process]),
    gen_echo:stop(Process).

%% @doc Initialize the echo server... nothing really.
%% @private Only gen_echo should call this function.
%% @since 0.0.0
init(_Args) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, []}.

%% @doc The main function, returns what is passed as parameter.
%% @private Only gen_echo should call this function.
%% @since 0.0.0
echo(Data, State) ->
    %%io:fwrite("~w:echo(~w)~n", [?MODULE, Data]),
    %% {string:concat("You said: ", Data), State}. % Funnier version, but non-compliant.
    {Data, State}.

transports() ->
    [tcp, udp].

%% @doc Clean up.
%% @private Only gen_echo should call this function.
%% @since 0.0.0
terminate(_Reason, _State) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, _State]),
    ok.

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
%% @doc This is a chargen server implemented using gen_chargen of Serlvers (http://software.pupeno.com/Serlvers).
%% @see echo
%% @see daytime
%% @see time
%% @since 0.0.0

-module(chargen).
-behaviour(gen_chargen).
-export([start/0, start/1, start_link/0, start_link/1, stop/1]).
-export([init/1, chargen/1, terminate/2]).

-define(posChars, "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"). % Possible characters.
-define(lineLength, 71). % Length of the line to send.

%% @doc Start an unnamed chargen server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start() ->
    %%io:fwrite("~w:start()~n", [?MODULE]),
    gen_chargen:start(?MODULE, [], []).

%% @doc Start a named chargen server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(SupName) ->
    %%io:fwrite("~w:start(~w)~n", [?MODULE, SupName]),
    gen_chargen:start(SupName, ?MODULE, [], []).

%% @doc Start an unnamed chargen server.
%% @see start/0
%% @see start_link/1
%% @since 0.0.0
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link() ->
    %%io:fwrite("~w:start_link()~n", [?MODULE]),
    gen_chargen:start_link(?MODULE, [], []).

%% @doc Start a named chargen server.
%% @see start/1
%% @see start_link/0
%% @since 0.0.0
%% @spec (SupName) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(SupName) ->
    %%io:fwrite("~w:start_link(~w)~n", [?MODULE, SupName]),
    gen_chargen:start_link(SupName, ?MODULE, [], []).

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
    gen_chargen:stop(Process).

%% @doc Initialize the chargen server... nothing really.
%% @private Only gen_chargen should call this function.
%% @since 0.0.0
init(_Args) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, [0]}.

%% @doc The main function, generates and returns the characters.
%% @private Only gen_chargen should call this function.
%% @since 0.0.0
chargen([Position]) ->
    %%io:fwrite("~w:chargen([~w])~n", [?MODULE, Position]),
    Chargen = lists:append(circular_sublist(?posChars, Position, ?lineLength), "\n"),
    LastChar = length(?posChars) - 1,
    NewPosition = if Position >= LastChar  -> 0;
                     Position < LastChar -> Position + 1
                  end,
    {Chargen, [NewPosition]}.

%% @doc Clean up.
%% @private Only gen_chargen should call this function.
%% @since 0.0.0
terminate(_Reason, _State) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, _Reason, _State]),
    ok.

%% @doc This function is like lists:sublist/3 but treats the list as a circular list.
%% <p>So, even when the start should be within limits, the length can be anything and that legnth will be reached by starting from the begining of the list as much as needed.</p>
circular_sublist(List, Start, _Length) when Start < 0; Start >= length(List) ->
    %%io:fwrite("~w:circular_sublist(~w, ~w, ~w)~n", [?MODULE, List, Start, _Length]),
    error;
circular_sublist(List, Start, Length) when Start + Length =< length(List) ->
    %%io:fwrite("~w:circular_sublist(~w, ~w, ~w)~n", [?MODULE, List, Start, Length]),
    lists:sublist(List, Start + 1, Length); % Start + 1 because sublist considers the first item of a list to be 1, not 0.
circular_sublist(List, Start, Length) when Start + Length > length(List) ->
    %%io:fwrite("~w:circular_sublist(~w, ~w, ~w)~n", [?MODULE, List, Start, Length]),
    Rest = Length - (length(List) - Start), % Rest of items to return.
    lists:append(lists:nthtail(Start, List),
                 circular_sublist(List, 0, Rest)).

%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of Fanterlastic Four.
%%
%% Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking Fanterlastic Four statically or dynamically with other modules is making a combined work based on Fanterlastic Four. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of Fanterlastic Four give you permission to combine Fanterlastic Four program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Fanterlastic Four and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

-module(fanterlasticfour_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0, children/0]).
-export([init/1]).

start_link() ->
    %io:fwrite("~w:start_link()~n", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    case (whereis(echo_sup)) of
        undefined ->
            {ok, echo_sup_not_running};
        Pid ->
            {ok, exit(Pid, normal)}
    end.

children() ->
    %io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).

init(_Args) ->
    %io:fwrite("~w:init(~w)~n", [?MODULE, _Args]),
    {ok, EchoUDPPort} = application:get_env(echoUDPPort),
    {ok, EchoTCPPort} = application:get_env(echoTCPPort),
    {ok, ChargenUDPPort} = application:get_env(chargenUDPPort),
    {ok, ChargenTCPPort} = application:get_env(chargenTCPPort),
    {ok, DaytimeUDPPort} = application:get_env(daytimeUDPPort),
    {ok, DaytimeTCPPort} = application:get_env(daytimeTCPPort),
    {ok, TimeUDPPort} = application:get_env(timeUDPPort),
    {ok, TimeTCPPort} = application:get_env(timeTCPPort),
    {ok, PortOffset} = application:get_env(portOffset),
    {ok, {{one_for_one, 1, 5},
          [{echo_udp,
            {launcher, start_link, [{local, echo_udp_launcher},
                                    echo,
                                    udp,
                                    EchoUDPPort + PortOffset]},
            permanent, 1000, worker, [launcher]},
           {echo_tcp,
            {launcher, start_link, [{local, echo_tcp_launcher},
                                    echo,
                                    tcp,
                                    EchoTCPPort + PortOffset]},
            permanent, 1000, worker, [launcher]},
%           {chargen_udp,
%            {launcher, start_link, [{local, chargen_udp_launcher},
%                                    chargen,
%                                    udp,
%                                    ChargenUDPPort + PortOffset]},
%            permanent, 1000, worker, [launcher]},
%           {chargen_tcp,
%            {launcher, start_link, [{local, chargen_tcp_launcher},
%                                    chargen,
%                                    tcp,
%                                    ChargenTCPPort + PortOffset]},
%            permanent, 1000, worker, [launcher]},
           {daytime_udp,
            {launcher, start_link, [{local, daytime_udp_launcher},
                                    daytime,
                                    udp,
                                    DaytimeUDPPort + PortOffset]},
            permanent, 1000, worker, [launcher]},
           {daytime_tcp,
            {launcher, start_link, [{local, daytime_tcp_launcher},
                                    daytime,
                                    tcp,
                                    DaytimeTCPPort + PortOffset]},
            permanent, 1000, worker, [launcher]}%,
%           {time_udp,
%            {launcher, start_link, [{local, time_udp_launcher},
%                                    time,
%                                    udp,
%                                    TimeUDPPort + PortOffset]},
%            permanent, 1000, worker, [launcher]},
%           {time_tcp,
%            {launcher, start_link, [{local, time_tcp_launcher},
%                                    time,
%                                    tcp,
%                                    TimeTCPPort + PortOffset]},
%            permanent, 1000, worker, [launcher]}
]}}.

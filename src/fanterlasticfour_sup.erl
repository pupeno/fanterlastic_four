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
    %%io:fwrite("~w:start_link()~n", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    io:fwrite("~w:stop()~n", [?MODULE]),
    ?MODULE ! {'EXIT', self(), shutdown},
    ok.


children() ->
    %%io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).


init(Services) ->
    io:fwrite("~w:init(~w)~n", [?MODULE, Services]),
%    Cs = child_spec(Services),
%    io:fwrite(" Child specs: ~w.~n", [Cs]),
    {ok, {{one_for_one, 1, 5}, []}}.

%% @doc Turn a configuration list into a children specification list.
%% @since 0.1.0
%% TODO: write a correct spec, mark the function as private.
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
children_specs([]) ->
    io:fwrite("~w:children_specs([])~n", [?MODULE]),
    [];

children_specs([Service|Services]) ->
    io:fwrite("~w:children_specs(~w)~n", [?MODULE, [Service|Services]]),
    lists:append(children_specs(Service), children_specs(Services));

children_specs({Name}) ->
    io:fwrite("~w:children_specs(~w)~n", [?MODULE, {Name}]),
    children_specs({Name, [{default, all, default}]});

children_specs({Name, Interfaces}) ->
    io:fwrite("~w:children_specs(~w)~n", [?MODULE, {Name, Interfaces}]),
    lists:map(fun(I) -> child_spec(Name, I) end,
              lists:flatmap(fun(I) -> fill_defaults(Name, I) end,
                            explode_interfaces(Interfaces)));

children_specs(Name) ->
    io:fwrite("~w:children_specs(~w)~n", [?MODULE, Name]),
    children_specs({Name}).

%% @doc Turn a compact specification of interfaces (the triple port, ip, transport) into an expanded one where each item is only one transport, one ip (or all) and one port.
%% @since 0.1.0
%% TODO: write a correct spec, mark the function as private.
%% @spec () -> Result
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
explode_interfaces({Port, Ip, both}) ->
    lists:flatmap(fun explode_interfaces/1, [{Port, Ip, tcp},
                                             {Port, Ip, udp}]);

explode_interfaces({[],           _IP, _Transport}) ->
    [];
explode_interfaces({[Port|Ports],  Ip,  Transport}) ->
    lists:append(explode_interfaces({Port,  Ip, Transport}),
                 explode_interfaces({Ports, Ip, Transport}));

explode_interfaces({_Port, [],       _Transport}) ->
    [];
%%explode_interfaces({ Port, [[_|_]=Ip|Ips],  Transport}) ->
explode_interfaces({ Port, [Ip|Ips],  Transport}) when is_list(Ip) ->
    lists:append(explode_interfaces({Port, Ip,  Transport}),
                 explode_interfaces({Port, Ips, Transport}));

explode_interfaces({_Port, _Ip, []}) ->
    [];
explode_interfaces({ Port,  Ip, [Transport|Transports]}) ->
    lists:append(explode_interfaces({Port, Ip, Transport}),
                 explode_interfaces({Port, Ip, Transports}));

explode_interfaces({Port, Ip, Transport}) ->
    [{Port, Ip, Transport}];

explode_interfaces(Interfaces) when is_list(Interfaces) ->
    lists:flatmap(fun explode_interfaces/1, Interfaces).


%% @doc Having a interface definition with some missing parts or some default parts turn in into a interface definition with all parts defined to the real value.
%% @since 0.1.0
%% TODO: write a correct spec, mark the function as private.
%% @spec (Name, Interface) -> {Port, Ip, Transport}.
fill_defaults(Name, {}) ->
    fill_defaults(Name, {default});

fill_defaults(Name, {Port}) ->
    fill_defaults(Name, {Port, all});

fill_defaults(Name, {Port, Ip}) ->
    fill_defaults(Name, {Port, Ip, default});

fill_defaults(Name, {Port, Ip, default}) ->
    lists:map(fun(Transport) -> fill_defaults(Name, {Port, Ip, Transport}) end, Name:transports());

fill_defaults(Name, {default, Ip, Transport}) ->
    {ok, Port} = inet:getservbyname(Name, Transport),
    fill_defaults(Name, {Port, Ip, Transport});

fill_defaults(_Name, {Port, Ip, Transport}) ->
    {Port, Ip, Transport}.


%% @doc Shortcut to call fill_defaults(Name, {}) by just providing the first parameter.
%% @since 0.1.0
%% TODO: write a correct spec, mark the function as private.
%% @spec (Name) -> {Port, Ip, Transport}.
fill_defaults(Name) ->
    fill_defaults(Name, {}).

%% @doc Having an interface definition, turn it into a child specification for the supervisor.
%% @since 0.1.0
%% TODO: write a correct spec, mark the function as private.
%% @spec (Name, {Port, Ip, Transport) -> Result
%%   Result = {Id, {launcher, start_link, [{local, ProcName}, Name, Transport, Port]}, permanent, 1000, worker, [launcher]}.
child_spec(Name, {Port, Ip, Transport}) ->
    io:fwrite("~w:child_spec(~w, ~w)~n", [?MODULE, Name, {Port, Ip, Transport}]),
    BaseName = lists:append([atom_to_list(Name), "_", atom_to_list(Transport), "_",
                             if is_list(Ip) -> Ip;
                                is_atom(Ip) -> atom_to_list(Ip)
                             end,
                             "_",integer_to_list(Port)]),
    io:fwrite("  BaseName=~w~n", [BaseName]),
    Id = list_to_atom(BaseName),
    io:fwrite("  Id=~w~n", [Id]),
    ProcName = list_to_atom(lists:append(BaseName, "_launcher")),
    io:fwrite("  ProcName=~w~n", [ProcName]),
    {Id,
     {launcher, start_link, [{local, ProcName}, Name, Transport, Port]},
     permanent, 1000, worker, [launcher]}.


    %% {ok, EchoUDPPort} = application:get_env(echoUDPPort),
%%     {ok, EchoTCPPort} = application:get_env(echoTCPPort),
%%     {ok, ChargenUDPPort} = application:get_env(chargenUDPPort),
%%     {ok, ChargenTCPPort} = application:get_env(chargenTCPPort),
%%     {ok, DaytimeUDPPort} = application:get_env(daytimeUDPPort),
%%     {ok, DaytimeTCPPort} = application:get_env(daytimeTCPPort),
%%     {ok, TimeUDPPort} = application:get_env(timeUDPPort),
%%     {ok, TimeTCPPort} = application:get_env(timeTCPPort),
%%     {ok, PortOffset} = application:get_env(portOffset),
%%     {ok, {{one_for_one, 1, 5},
%%           [{echo_udp,
%%             {launcher, start_link, [{local, echo_udp_launcher},
%%                                     echo,
%%                                     udp,
%%                                     EchoUDPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {echo_tcp,
%%             {launcher, start_link, [{local, echo_tcp_launcher},
%%                                     echo,
%%                                     tcp,
%%                                     EchoTCPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {chargen_udp,
%%             {launcher, start_link, [{local, chargen_udp_launcher},
%%                                     chargen,
%%                                     udp,
%%                                     ChargenUDPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {chargen_tcp,
%%             {launcher, start_link, [{local, chargen_tcp_launcher},
%%                                     chargen,
%%                                     tcp,
%%                                     ChargenTCPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {daytime_udp,
%%             {launcher, start_link, [{local, daytime_udp_launcher},
%%                                     daytime,
%%                                     udp,
%%                                     DaytimeUDPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {daytime_tcp,
%%             {launcher, start_link, [{local, daytime_tcp_launcher},
%%                                     daytime,
%%                                     tcp,
%%                                     DaytimeTCPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {time_udp,
%%             {launcher, start_link, [{local, time_udp_launcher},
%%                                     time,
%%                                     udp,
%%                                     TimeUDPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]},
%%            {time_tcp,
%%             {launcher, start_link, [{local, time_tcp_launcher},
%%                                     time,
%%                                     tcp,
%%                                     TimeTCPPort + PortOffset]},
%%             permanent, 1000, worker, [launcher]}]}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

child_spec_echo_1234_all_tcp_test() ->
    {echo_tcp_all_1234,{launcher, start_link,
                        [{local, echo_tcp_all_1234_launcher}, echo, tcp, 1234]},
     permanent, 1000, worker, [launcher]}
        = child_spec(echo, {1234, all, tcp}).

child_spec_chargen_10000_10_0_0_1_udp_test() ->
    {'chargen_udp_10.0.0.1_10000',{launcher, start_link,
                        [{local,'chargen_udp_10.0.0.1_10000_launcher'}, chargen, udp, 10000]},
     permanent, 1000, worker, [launcher]}
        = child_spec(chargen, {10000, "10.0.0.1", udp}).
-endif.

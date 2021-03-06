%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of Fanterlastic Four.
%%
%% Fanterlastic Four is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% Fanterlastic Four is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Fanterlastic Four; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking Fanterlastic Four statically or dynamically with other modules is making a combined work based on Fanterlastic Four. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of Fanterlastic Four give you permission to combine Fanterlastic Four program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Fanterlastic Four and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%% @doc Supervisor for the Fanterlastic Four application.

-module(fanterlasticfour_sup).
-behaviour(supervisor).
-export([start_link/1, stop/0, children/0]).
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

%% @TODO Write documentation.
start_link(Services) ->
    %%io:fwrite("~w:start_link(~w)~n", [?MODULE, Services]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Services).


%% @TODO Write documentation.
stop() ->
    %%io:fwrite("~w:stop()~n", [?MODULE]),
    ?MODULE ! {'EXIT', self(), shutdown},
    ok.

%% @TODO Write documentation.
children() ->
    %%io:fwrite("~w:which_children()~n", [?MODULE]),
    supervisor:which_children(?MODULE).


%% @TODO Write documentation.
init(Services) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, Services]),
    Cs = children_specs(Services),
    %%io:fwrite("  Child specs: ~w.~n", [Cs]),
    {ok, {{one_for_one, 1, 5}, Cs}}.

%% @doc Turn a configuration list into a children specification list.
%% @private Internal helper function.
%% @since 0.2.0
children_specs({Name}) ->
    %%io:fwrite("~w:children_specs(~w)~n", [?MODULE, {Name}]),
    children_specs({Name, [{default, all, default}]});

children_specs({Name, Interfaces}) ->
    %%io:fwrite("~w:children_specs(~w)~n", [?MODULE, {Name, Interfaces}]),
    lists:map(fun(I) -> child_spec(Name, I) end,
              lists:flatmap(fun(I) -> fill_defaults(Name, I) end,
                            explode_interfaces(Interfaces)));

children_specs(Name) when is_atom(Name) ->
    %%io:fwrite("~w:children_specs(~w)~n", [?MODULE, Name]),
    children_specs({Name});

children_specs(Services) when is_list(Services) ->
    %%io:fwrite("~w:children_specs(~w)~n", [?MODULE, Services]),
    lists:flatmap(fun children_specs/1, Services).

%% @doc Turn a compact specification of interfaces (the triple port, ip, transport) into an expanded one where each item is only one transport, one ip (or all) and one port.
%% @private Internal helper function.
%% @since 0.2.0
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
explode_interfaces({ Port, [Ip|Ips],  Transport}) when is_list(Ip);    % An Ip is a list when it is a string with the ip, like: "10.0.0.1".
                                                       is_atom(Ip);    % An Ip is an atom when it is 'all', which is all the Ips.
                                                       is_tuple(Ip) -> % An Ip is a tuplu when it comes pre-parsed, like {10,0,0,1}.
    lists:append(explode_interfaces({Port, Ip,  Transport}),
                 explode_interfaces({Port, Ips, Transport}));

explode_interfaces({_Port, _Ip, []}) ->
    [];
explode_interfaces({ Port,  Ip, [Transport|Transports]}) ->
    lists:append(explode_interfaces({Port, Ip, Transport}),
                 explode_interfaces({Port, Ip, Transports}));

explode_interfaces({Port, Ip, Transport}) when is_list(Ip) ->
    {ok, ParsedIp} = inet_parse:address(Ip),
    [{Port, ParsedIp, Transport}];

explode_interfaces({Port, Ip, Transport}) ->
    [{Port, Ip, Transport}];

explode_interfaces(Interfaces) when is_list(Interfaces) ->
    lists:flatmap(fun explode_interfaces/1, Interfaces).


%% @doc Having a interface definition with some missing parts or some default parts turn in into a interface definition with all parts defined to the real value.
%% @private Internal helper function.
%% @since 0.2.0
fill_defaults(Name, {}) ->
    fill_defaults(Name, {default});

fill_defaults(Name, {Port}) ->
    fill_defaults(Name, {Port, all});

fill_defaults(Name, {Port, Ip}) ->
    fill_defaults(Name, {Port, Ip, default});

fill_defaults(Name, {Port, Ip, default}) ->
    lists:flatmap(fun(Transport) -> fill_defaults(Name, {Port, Ip, Transport}) end, Name:transports());

fill_defaults(Name, {default, Ip, Transport}) ->
    {ok, Port} = inet:getservbyname(Name, Transport),
    fill_defaults(Name, {Port, Ip, Transport});

fill_defaults(_Name, {Port, Ip, Transport}) ->
    [{Port, Ip, Transport}].


%% @doc Shortcut to call fill_defaults(Name, {}) by just providing the first parameter.
%% @private Internal helper function.
%% @since 0.2.0
fill_defaults(Name) ->
    fill_defaults(Name, {}).


%% @doc Having an interface definition, turn it into a child specification for the supervisor.
%% @private Internal helper function.
%% @since 0.2.0
child_spec(Name, {Port, Ip, Transport}) ->
    %%io:fwrite("~w:child_spec(~w, ~w)~n", [?MODULE, Name, {Port, Ip, Transport}]),
    BaseName = lists:append([atom_to_list(Name), "_", atom_to_list(Transport), "_",
                             if is_tuple(Ip) -> join(lists:map(fun(X) -> integer_to_list(X) end,
                                                               tuple_to_list(Ip)),
                                                     "_");
                                is_atom(Ip) -> atom_to_list(Ip)
                             end,
                             "_",integer_to_list(Port)]),
    Id = list_to_atom(BaseName),
    ProcName = list_to_atom(lists:append(BaseName, "_launcher")),
    {Id,
     {launcher, start_link, [{local, ProcName}, Name, Transport, Ip, Port]},
     permanent, 1000, worker, [launcher]}.

%% @doc Having list of strings and a separator, join or implode into one string with all the strings from the list separated by the separator.
%% @private Internal helper function.
%% @since 0.2.0
join(Strs, Separator) ->
    lists:foldr(fun(Str,[]) -> Str; (Str, Acc) -> Str ++ Separator ++ Acc end, "", Strs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

children_specs_test_() ->
    [?_assert(children_specs([echo, {chargen, {1765, all, tcp}}]) ==
              [{echo_tcp_all_7,
                {launcher, start_link, [{local, echo_tcp_all_7_launcher}, echo, tcp, all, 7]},
                permanent, 1000, worker, [launcher]},
               {echo_udp_all_7,
                {launcher, start_link, [{local, echo_udp_all_7_launcher}, echo, udp, all, 7]},
                permanent, 1000, worker, [launcher]},
               {chargen_tcp_all_1765,
                {launcher, start_link, [{local, chargen_tcp_all_1765_launcher}, chargen, tcp, all, 1765]},
                permanent, 1000, worker, [launcher]}]),
     ?_assert(children_specs({echo}) ==
              [{echo_tcp_all_7,
                {launcher, start_link, [{local, echo_tcp_all_7_launcher}, echo, tcp, all, 7]},
                permanent, 1000, worker, [launcher]},
               {echo_udp_all_7,
                {launcher, start_link, [{local, echo_udp_all_7_launcher}, echo, udp, all, 7]},
                permanent, 1000, worker, [launcher]}]),
     ?_assert(children_specs({echo, [{1234, all, tcp}]}) ==
              [{echo_tcp_all_1234,
                {launcher, start_link, [{local, echo_tcp_all_1234_launcher}, echo, tcp, all, 1234]},
                permanent, 1000, worker, [launcher]}]),
     ?_assert(children_specs(echo) == children_specs({echo}))].


explode_interfaces_test_() ->
    Ip = "127.0.0.1",
    {ok, ParsedIp} = inet_parse:address(Ip),
    Ip2 = "2002::1",
    {ok, ParsedIp2} = inet_parse:address(Ip2),
    [?_assert(explode_interfaces({1234, Ip, both}) == [{1234, ParsedIp, tcp}, {1234, ParsedIp, udp}]),
     ?_assert(explode_interfaces({[7,8], Ip, udp}) == [{7, ParsedIp, udp}, {8, ParsedIp, udp}]),
     ?_assert(explode_interfaces({1234, [Ip, Ip2], udp}) == [{1234, ParsedIp, udp}, {1234, ParsedIp2, udp}]),
     ?_assert(explode_interfaces({1234, Ip, [tcp,udp]}) == [{1234, ParsedIp, tcp}, {1234, ParsedIp, udp}]),
     ?_assert(explode_interfaces({1234, Ip, tcp}) == [{1234, ParsedIp, tcp}]),
     ?_assert(explode_interfaces({1234, ParsedIp, tcp}) == [{1234, ParsedIp, tcp}]),
     ?_assert(explode_interfaces([{7, Ip, both}, {[9,2], [Ip, ParsedIp2, all], [tcp, udp]}]) ==
              [{7, ParsedIp, tcp}, {7, ParsedIp, udp},
               {9, ParsedIp, tcp}, {9, ParsedIp, udp}, {9, ParsedIp2, tcp}, {9, ParsedIp2, udp}, {9, all, tcp}, {9, all, udp},
               {2, ParsedIp, tcp}, {2, ParsedIp, udp}, {2, ParsedIp2, tcp}, {2, ParsedIp2, udp}, {2, all, tcp}, {2, all, udp}])].

fill_defaults_test_() ->
    Ip = "127.0.0.1",
    [?_assert(fill_defaults(echo) == [{7, all, tcp}, {7, all, udp}]),
     ?_assert(fill_defaults(echo, {}) == [{7, all, tcp}, {7, all, udp}]),
     ?_assert(fill_defaults(echo, {1543}) == [{1543, all, tcp}, {1543, all, udp}]),
     ?_assert(fill_defaults(echo, {1543, Ip}) == [{1543, Ip, tcp}, {1543, Ip, udp}]),
     ?_assert(fill_defaults(echo, {1543, Ip, default}) == [{1543, Ip, tcp}, {1543, Ip, udp}]),
     ?_assert(fill_defaults(echo, {default, Ip, tcp}) == [{7, Ip, tcp}]),
     ?_assert(fill_defaults(echo, {1543, Ip, tcp}) == [{1543, Ip, tcp}])].


child_spec_test_() ->
    [?_assert(child_spec(echo, {1234, all, tcp}) ==
              {echo_tcp_all_1234,
               {launcher, start_link, [{local, echo_tcp_all_1234_launcher}, echo, tcp, all, 1234]},
               permanent, 1000, worker, [launcher]}),
     ?_assert(child_spec(chargen, {15432, {10,0,0,1}, udp}) ==
              {'chargen_udp_10_0_0_1_15432',
               {launcher, start_link, [{local,'chargen_udp_10_0_0_1_15432_launcher'}, chargen, udp, {10,0,0,1}, 15432]},
               permanent, 1000, worker, [launcher]}),
     ?_assert(child_spec(time, {7654, {8194,0,0,0,0,0,0,1}, udp}) ==
              {'time_udp_8194_0_0_0_0_0_0_1_7654',
               {launcher, start_link, [{local,'time_udp_8194_0_0_0_0_0_0_1_7654_launcher'}, time, udp, {8194,0,0,0,0,0,0,1}, 7654]},
               permanent, 1000, worker, [launcher]})].

join_test_() ->
    [?_assert(join(["a", "b", "c"], ",") == "a,b,c")].


-endif. %% ifdef(TEST).

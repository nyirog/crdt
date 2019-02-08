-module(crdt_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([nodes_are_connected/1, add_is_propagated/1, remove_is_propagated/1]).

all() -> [nodes_are_connected, {group, actions}].

groups() ->
    [{actions, [shuffle, {repeat, 5}], [add_is_propagated, remove_is_propagated]}].

init_per_testcase(_TestName, Config) ->
    {ok, PidA} = crdt_server:start_link(a),
    {ok, PidB} = crdt_server:start_link(b),
    {ok, PidC} = crdt_server:start_link(c),
    crdt_server:connect(PidA, PidB),
    crdt_server:connect(PidB, PidC),
    [{pid_a, PidA} | [{pid_b, PidB} | [{pid_c, PidC} | Config]]].

end_per_testcase(_TestName, Config) ->
    crdt_server:stop(?config(pid_a, Config)),
    crdt_server:stop(?config(pid_b, Config)),
    crdt_server:stop(?config(pid_c, Config)).

nodes_are_connected(Config) ->
    Nodes = crdt_server:nodes(?config(pid_c, Config)),
    true = lists:member(?config(pid_a, Config), Nodes),
    true = lists:member(?config(pid_b, Config), Nodes).

add_is_propagated(Config) ->
   Member = rand:uniform(),
   crdt_server:add(?config(pid_b, Config), Member),
   timer:sleep(100),
   true = crdt_server:member(?config(pid_c, Config), Member).

remove_is_propagated(Config) ->
   Member = rand:uniform(),
   crdt_server:add(?config(pid_a, Config), Member),
   timer:sleep(100),
   crdt_server:remove(?config(pid_b, Config), Member),
   timer:sleep(100),
   false = crdt_server:member(?config(pid_c, Config), Member).

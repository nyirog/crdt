-module(crdt_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([nodes_are_connected/1, add_is_propagated/1, remove_is_propagated/1]).

all() -> [nodes_are_connected, {group, actions}].

groups() ->
    [{actions, [shuffle, {repeat, 5}], [add_is_propagated, remove_is_propagated]}].

init_per_testcase(_TestName, _Config) ->
    crdt:install([], [pid_a, pid_b, pid_c]),
    mnesia:wait_for_tables([pid_a, pid_b, pid_c], 1000),
    {ok, _} = crdt_server:start_link(pid_a),
    {ok, _} = crdt_server:start_link(pid_b),
    {ok, _} = crdt_server:start_link(pid_c),
    crdt_server:connect(pid_a, pid_b),
    crdt_server:connect(pid_b, pid_c),
    [].

end_per_testcase(_TestName, _Config) ->
    crdt_server:stop(pid_a),
    crdt_server:stop(pid_b),
    crdt_server:stop(pid_c),
    lists:foreach(fun mnesia:delete_table/1, [pid_a, pid_b, pid_c]).

nodes_are_connected(_Config) ->
    timer:sleep(100),
    Nodes = crdt_server:nodes(pid_c),
    true = lists:member(pid_a, Nodes),
    true = lists:member(pid_b, Nodes).

add_is_propagated(_Config) ->
    Member = rand:uniform(),
    crdt_server:add(pid_b, Member),
    timer:sleep(100),
    true = crdt_server:member(pid_c, Member).

remove_is_propagated(_Config) ->
    Member = rand:uniform(),
    crdt_server:add(pid_a, Member),
    timer:sleep(100),
    crdt_server:remove(pid_b, Member),
    timer:sleep(100),
    false = crdt_server:member(pid_c, Member).

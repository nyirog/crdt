-module(crdt_cluster_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%====================================================================
%% Tests descriptions
%%====================================================================

crdt_cluster_test_() ->
    [{"connect register all the cluster nodes",
      ?setup(fun connect_registers_all_the_cluster_nodes/1)},
     {"add is propagated between cluster nodes",
      ?setup(fun add_is_propagated/1)},
     {"remove is propagated between cluster nodes",
      ?setup(fun remove_is_propagated/1)}].

%%====================================================================
%% Tests functions
%%====================================================================

start() ->
    crdt:init([], [a, b, c]),
    mnesia:wait_for_tables([a, b, c], 1000),
    {ok, _} = crdt_server:start_link(a),
    {ok, _} = crdt_server:start_link(b),
    {ok, _} = crdt_server:start_link(c),
    crdt_server:connect(a, b),
    crdt_server:connect(b, c),
    undef.

stop(_) ->
    lists:foreach(fun crdt_server:stop/1, [a, b, c]),
    lists:foreach(fun mnesia:delete_table/1, [a, b, c]).

%%====================================================================
%% Tests
%%====================================================================

connect_registers_all_the_cluster_nodes(_) ->
    timer:sleep(5),
    [?_assertNot(lists:member(c, crdt_server:nodes(c))),
     ?_assert(lists:member(a, crdt_server:nodes(c))),
     ?_assert(lists:member(b, crdt_server:nodes(c)))].

add_is_propagated(_) ->
    timer:sleep(5),
    crdt_server:add(b, 42),
    timer:sleep(5),
    [?_assert(crdt_server:member(a, 42)),
     ?_assert(crdt_server:member(c, 42))].

remove_is_propagated(_) ->
    timer:sleep(5),
    crdt_server:add(a, 42),
    timer:sleep(5),
    crdt_server:add(a, 24),
    timer:sleep(5),
    crdt_server:remove(a, 42),
    timer:sleep(5),
    [?_assertNot(crdt_server:member(b, 42)),
     ?_assert(crdt_server:member(b, 24)),
     ?_assertNot(crdt_server:member(c, 42)),
     ?_assert(crdt_server:member(c, 24))].

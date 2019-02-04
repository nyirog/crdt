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
    {ok, PidA} = crdt_server:start_link(),
    {ok, PidB} = crdt_server:start_link(),
    {ok, PidC} = crdt_server:start_link(),
    crdt_server:connect(PidA, PidB),
    crdt_server:connect(PidB, PidC),
    #{a => PidA, b => PidB, c => PidC}.

stop(Nodes) ->
    lists:foreach(fun crdt_server:stop/1, maps:values(Nodes)).

%%====================================================================
%% Tests
%%====================================================================

connect_registers_all_the_cluster_nodes(#{a := PidA, b := PidB, c := PidC}) ->
    [?_assertNot(lists:member(PidC, crdt_server:nodes(PidC))),
     ?_assert(lists:member(PidA, crdt_server:nodes(PidC))),
     ?_assert(lists:member(PidB, crdt_server:nodes(PidC)))].

add_is_propagated(#{a := PidA, b := PidB, c := PidC}) ->
    timer:sleep(1),
    crdt_server:add(PidB, 42),
    timer:sleep(1),
    [?_assert(crdt_server:member(PidA, 42)),
     ?_assert(crdt_server:member(PidC, 42))].

remove_is_propagated(#{a := PidA, b := PidB, c := PidC}) ->
    timer:sleep(1),
    crdt_server:add(PidA, 42),
    timer:sleep(1),
    crdt_server:add(PidA, 24),
    timer:sleep(1),
    crdt_server:remove(PidA, 42),
    timer:sleep(1),
    [?_assertNot(crdt_server:member(PidB, 42)),
     ?_assert(crdt_server:member(PidB, 24)),
     ?_assertNot(crdt_server:member(PidC, 42)),
     ?_assert(crdt_server:member(PidC, 24))].

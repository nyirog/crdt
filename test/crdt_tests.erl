-module(crdt_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%====================================================================
%% Tests descriptions
%%====================================================================

crdt_test_() ->
    [{"crdt looks like an empty list after init",
      ?setup(fun list_initial_value/1)},
     {"different values can be added to crdt",
      ?setup(fun addition/1)},
     {"crdt habdles addition like set",
      ?setup(fun repeat_addition/1)},
     {"repeated values can be deleted with one remove",
      ?setup(fun remove_repeated_value/1)}].

%%====================================================================
%% Tests functions
%%====================================================================

start() ->
    {ok, Pid} = crdt_server:start_link(),
    Pid.

stop(_) ->
    crdt_server:stop().

%%====================================================================
%% Tests
%%====================================================================

list_initial_value(_) ->
    [?_assertEqual(sets:new(), crdt:members())].

addition(_) ->
    crdt:add(6),
    crdt:add(9),
    [?_assert(crdt:member(6)),
     ?_assert(crdt:member(9)),
     ?_assertEqual(sets:from_list([6, 9]), crdt:members())].


repeat_addition(_) ->
    crdt:add(9),
    crdt:add(9),
    [?_assertEqual(sets:from_list([9]), crdt:members())].

remove_repeated_value(_) ->
    crdt:add(9),
    crdt:add(9),
    crdt:remove(9),
    [?_assertEqual(sets:new(), crdt:members())].

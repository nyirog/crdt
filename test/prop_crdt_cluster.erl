-module(prop_crdt_cluster).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/0, sample/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-define(SERVER, crdt_server).
-define(NODES, [node_a, node_b, node_c]).

-record(state, {nodes, members}).

test() ->
    proper:quickcheck(?MODULE:prop_crdt_cluster()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop_crdt_server_cluster() ->
    ?FORALL(Commands, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(node_a),
                    ?SERVER:start_link(node_b),
                    ?SERVER:start_link(node_c),
                    {History, State, Result} = run_commands(?MODULE, Commands),
                    Members = maps:from_list(
                                  lists:map(
                                      fun (Node) ->
                                         {Node, ordsets:from_list(?SERVER:members(Node))}
                                      end,
                                      ?NODES
                                  )
                              ),
                    ?SERVER:stop(node_a),
                    ?SERVER:stop(node_b),
                    ?SERVER:stop(node_c),
                    ?WHENFAIL(
                        io:format(
                            "History: ~w~nState: ~w\nResult: ~w~nMembers: ~w~n",
                            [History, State, Result, Members]
                        ),
                        aggregate(
                            command_names(Commands),
                            lists:all(
                                fun (Node) ->
                                   State#state.members =:= maps:get(Node, Members)
                                end,
                                State#state.nodes
                            )
                        )
                    )
                end)
            ).


initial_state() ->
    lists:foreach(fun mnesia:delete_table/1, [node_a, node_b, node_c]),
    crdt:install([], [node_a, node_b, node_c]),
    mnesia:wait_for_tables([node_a, node_b, node_c], 1000),
    #state{nodes = ordsets:new(), members = ordsets:new()}.

command(_S) ->
    oneof([{call, ?SERVER, add, [any_node(), member()]},
           {call, ?SERVER, remove, [any_node(), member()]},
           {call, ?SERVER, connect, [any_node(), any_node()]}]).

precondition(_, {call, _, connect, [NodeA, NodeB]}) -> NodeA =/= NodeB;
precondition(S, {call, _, add, [Node, _]}) -> ordsets:is_element(Node, S#state.nodes);
precondition(S, {call, _, remove, [Node, _]}) -> ordsets:is_element(Node, S#state.nodes).

next_state(S, _Result, {call, _, connect, Nodes}) ->
    timer:sleep(3),
    S#state{nodes = ordsets:union(Nodes, S#state.nodes)};

next_state(S, _Result, {call, _, add, [_Node, Member]}) ->
    timer:sleep(3),
    S#state{members = ordsets:add_element(Member, S#state.members)};

next_state(S, _Result, {call, _, remove, [_Node, Member]}) ->
    timer:sleep(3),
    S#state{members = ordsets:del_element(Member, S#state.members)}.

postcondition(_State, _Command, _Result) -> true.

member() -> elements(lists:seq(1, 5)).

any_node() -> elements(?NODES).

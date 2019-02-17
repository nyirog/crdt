-module(prop_crdt).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/0, sample/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-define(SERVER, crdt_server).

test() ->
    proper:quickcheck(?MODULE:prop_crdt_server_acts_like_a_set()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop_crdt_server_acts_like_a_set() ->
    ?FORALL(Commands, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(?SERVER),
                    {History, State, Result} = run_commands(?MODULE, Commands),
                    Members = ?SERVER:members(?SERVER),
                    ?SERVER:stop(?SERVER),
                    ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~w~nMembers: ~w~n",
                                        [History, State, Result, Members]),
                              aggregate(command_names(Commands),
                              ordsets:from_list(Members) =:= State))
                end)
            ).

initial_state() ->
    mnesia:delete_table(?SERVER),
    crdt:init([], [?SERVER]),
    mnesia:wait_for_tables([?SERVER], 1000),
    ordsets:new().

command(_State) ->
    oneof([{call, ?SERVER, add, [?SERVER, member()]},
           {call, ?SERVER, remove, [?SERVER, member()]}]).

precondition(_State, _Command) -> true.

next_state(State, _Result, {call, _, add, [_Pid, Member]}) ->
    ordsets:add_element(Member, State);

next_state(State, _Result, {call, _, remove, [_Pid, Member]}) ->
    ordsets:del_element(Member, State).

postcondition(_State, _Command, _Result) -> true.

member() -> elements(lists:seq(1, 5)).

%%%-----------------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-----------------------------------------------------------------------------

-include_lib("stdlib/include/qlc.hrl").

-module(crdt_server).

-behaviour(gen_server).

-include("crdt_server.hrl").

%% Application callbacks
-export([add/2, connect/2, member/2, members/1, nodes/1,
         remove/2, start_link/1, stop/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%%==============================================================================
%% API
%%==============================================================================

start_link(ServerRef = {Name, _Node}) ->
    gen_server:start_link({local, Name}, ?MODULE, init_state(ServerRef), []);
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, init_state(Name), []).

stop(Pid) -> gen_server:call(Pid, stop).

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------

handle_call(members, _From, State = #{node := Self}) ->
    {reply, lists:usort(list_values(get_table_name(Self))), State};
handle_call({member, Value}, _From, State = #{node := Self}) ->
    {reply, has_value(get_table_name(Self), Value), State};
handle_call(nodes, _From, State = #{nodes := Nodes}) ->
    {reply, Nodes, State};
handle_call({join, NewNode}, From, State = #{clock := Clock, node := Self}) ->
    [LeftClock, RightClock] = itc:fork(Clock),
    gen_server:reply(From, RightClock),
    gen_server:cast(
        NewNode,
        {sync, #event_key{node = Self, clock = itc:seed()}}
    ),
    NewEvent = #event{
        key = #event_key{clock = itc:event(LeftClock), node = Self},
        action = join,
        value = [NewNode, Self]
    },
    NewState = handle_event(NewEvent, State),
    sync_event(NewEvent, NewState),
    {noreply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Value}, State = #{clock := Clock, node := Node}) ->
    Event = #event{
        key = #event_key{clock = itc:event(Clock), node = Node},
        action = add,
        value = Value
    },
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({remove, Value}, State = #{clock := Clock, node := Self}) ->
    Event = #event{
        key = #event_key{clock = itc:event(Clock), node = Self},
        action = remove,
        value = filter_event_by_value(get_table_name(Self), Value)
    },
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({update, Event}, State = #{clock := Clock}) ->
    NewState = handle_event(Event, State),
    NewClock = itc:event(itc:join(maps:get(clock, NewState), Clock)),
    {noreply, NewState#{clock := NewClock}};
handle_cast(
        {sync, #event_key{node = Node, clock = Clock}},
        State = #{node := Self, nodes := Nodes}
) ->
    TableName = get_table_name(Self),
    Events = fun () -> qlc:e(qlc:q([E || E <- mnesia:table(TableName)])) end,
    UnseenEvents = case lists:member(Node, Nodes) of
        true -> list_unseen_events(TableName, Clock);
        false -> mnesia:async_dirty(Events)
    end,
    update_events(Node, UnseenEvents),
    LastSeenClock = get_last_seen_event_clock(get_table_name(Self), Clock),
    gen_server:cast(
        Node,
        {sync_from, #event_key{node = Self, clock = LastSeenClock}}
    ),
    {noreply, State};
handle_cast(
        {sync_from, #event_key{node = Node, clock = Clock}},
        State = #{node := Self}
) ->
    update_events(Node, list_unseen_events(get_table_name(Self), Clock)),
    {noreply, State};
handle_cast({connect, Node}, State = #{nodes := Nodes, node := Self}) ->
    TableName = get_table_name(Self),
    Events = fun () -> qlc:e(qlc:q([E || E <- mnesia:table(TableName)])) end,
    update_events(Node, mnesia:async_dirty(Events)),
    {
        noreply,
        State#{
            clock := gen_server:call(Node, {join, Self}),
            nodes := add_node(Node, Self, Nodes)
        }
    };
handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%------------------------------------------------------------------------------

add(Pid, Value) -> gen_server:cast(Pid, {add, Value}).

remove(Pid, Value) ->
    gen_server:cast(Pid, {remove, Value}).

connect(Pid, Node) ->
    gen_server:cast(Pid, {connect, Node}).

members(Pid) -> gen_server:call(Pid, members).

member(Pid, Value) ->
    gen_server:call(Pid, {member, Value}).

nodes(Pid) -> gen_server:call(Pid, nodes).

%%==============================================================================
%% Internal functions
%%==============================================================================

init_state(ServerRef) ->
    Clock = itc:seed(),
    EventKey = #event_key{clock = Clock, node = ServerRef},
    Event = #event{action = init, key = EventKey, value = none},
    add_event(get_table_name(ServerRef), Event),
    #{nodes => init_nodes(ServerRef), clock => Clock, node => ServerRef}.

sync_event(#event{key = Key}, #{nodes := Nodes, node := Self}) ->
    Sync = fun (Pid) ->
        gen_server:cast(Pid, {sync, Key#event_key{node = Self}})
    end,
    lists:foreach(Sync, Nodes).

handle_event(#event{action = init}, State) -> State;
handle_event(
        Event = #event{
            action = join,
            key = #event_key{clock = Clock},
            value = [LeftNode, RightNode]
        },
        State = #{nodes := Nodes, node := Self}
) ->
    add_event(get_table_name(Self), Event),
    State#{clock := Clock,
           nodes := add_node(
               RightNode,
               Self,
               add_node(LeftNode, Self, Nodes)
           )
    };
handle_event(
        Event = #event{action = add, key = #event_key{clock = Clock}},
        State = #{node := Self}
) ->
    add_event(get_table_name(Self), Event),
    State#{clock := Clock};
handle_event(
        Event = #event{
            action = remove,
            value = Removables,
            key = #event_key{clock = Clock}
        },
        State = #{node := Self}
) ->
    TableName = get_table_name(Self),
    Remove = fun () ->
        lists:foreach(
            fun (K) -> mnesia:dirty_delete(TableName, K) end,
            Removables),
        mnesia:dirty_write(TableName, Event)
    end,
    mnesia:async_dirty(Remove),
    State#{clock := Clock}.

init_nodes(Self) ->
    TableName = get_table_name(Self),
    JoinQuery = fun() ->
        Q = qlc:q([
            E#event.value
            || E <- mnesia:table(TableName),
            E#event.action =:= join
        ]),
        qlc:e(Q)
    end,
    AllNodes = lists:flatten(mnesia:async_dirty(JoinQuery)),
    lists:foldl(
        fun (Node, Nodes) -> add_node(Node, Self, Nodes) end,
        [],
        AllNodes
    ).

add_node(Node, Self, Nodes) ->
    if Node =:= Self -> Nodes;
       true -> lists:usort([Node | Nodes])
    end.

update_events(Node, Events) ->
    Update = fun (Event) -> gen_server:cast(Node, {update, Event}) end,
    lists:foreach(Update, Events).

add_event(TableName, Event) ->
    mnesia:dirty_write(TableName, Event).

list_values(TableName) ->
    Members = fun () ->
        Q = qlc:q([
            E#event.value
            || E <- mnesia:table(TableName),
            E#event.action =:= add
        ]),
        qlc:e(Q)
    end,
    mnesia:async_dirty(Members).

has_value(TableName, Value) ->
    Members = fun () ->
        Q = qlc:q([E || E <- mnesia:table(TableName), E#event.value =:= Value]),
        qlc:e(Q)
    end,
    erlang:length(mnesia:async_dirty(Members)) > 0.

list_unseen_events(TableName, Clock) ->
    Events = fun () ->
        Q = qlc:q([
            E
            || E <- mnesia:table(TableName),
            not itc:leq((E#event.key)#event_key.clock, Clock)
        ]),
        qlc:e(Q)
    end,
    mnesia:async_dirty(Events).

get_last_seen_event_clock(TableName, Clock) ->
    Clocks = fun () ->
        Q = qlc:q([
            C
            || #event{key = #event_key{clock = C}} <- mnesia:table(TableName),
            itc:leq(C, Clock)
        ]),
        qlc:e(Q)
    end,
    Cmp = fun (C, D) -> itc:leq(D, C) end,
    case mnesia:async_dirty(Clocks) of
        [] -> itc:seed();
        [Last] -> Last;
        FilteredClocks -> erlang:hd(lists:sort(Cmp, FilteredClocks))
    end.

filter_event_by_value(TableName, Value) ->
    Keys = fun () ->
        Q = qlc:q([
            K
            || #event{key = K, value = EventValue} <- mnesia:table(TableName),
            EventValue =:= Value
        ]),
        qlc:e(Q)
    end,
    mnesia:async_dirty(Keys).

get_table_name({Name, _Node}) -> Name;
get_table_name(Name) -> Name.

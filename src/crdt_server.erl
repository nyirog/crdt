%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(event, {action, node, clock, value}).

%% Application callbacks
-export([add/2, connect/2, member/2, members/1, nodes/1,
         remove/2, start_link/1, stop/0, stop/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%%====================================================================
%% API
%%====================================================================

start_link(ServerRef = {Name, _Node}) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          init_state(ServerRef), []);
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          init_state(Name), []).

stop(Pid) -> gen_server:call(Pid, stop).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------

handle_call(members, _From,
            State = #{history := History}) ->
    {reply, lists:usort(list_members(History)), State};
handle_call({member, Value}, _From,
            State = #{history := History}) ->
    {reply, lists:member(Value, list_members(History)),
     State};
handle_call(nodes, _From, State = #{nodes := Nodes}) ->
    {reply, Nodes, State};
handle_call({join, NewNode}, From,
            State = #{clock := Clock, node := Self}) ->
    [LeftClock, RightClock] = itc:fork(Clock),
    gen_server:reply(From, RightClock),
    NewEvent = #event{clock = itc:event(LeftClock),
                      node = Self, action = join, value = [NewNode, Self]},
    NewState = handle_event(NewEvent, State),
    sync_event(NewEvent, NewState),
    {noreply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Value},
            State = #{clock := Clock, node := Node}) ->
    Event = #event{clock = itc:event(Clock), action = add,
                   node = Node, value = Value},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({remove, Value},
            State = #{clock := Clock, node := Node,
                      history := History}) ->
    Event = #event{clock = itc:event(Clock),
                   action = remove, node = Node,
                   value = filter_event_by_value(Value, History)},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({update, Event},
            State = #{clock := Clock}) ->
    NewState = handle_event(Event, State),
    NewClock = itc:event(itc:join(maps:get(clock, NewState),
                                  Clock)),
    {noreply, NewState#{clock := NewClock}};
handle_cast({sync, Node, Clock},
            State = #{node := Self, nodes := Nodes,
                      history := History}) ->
    UnseenEvents = case lists:member(Node, Nodes) of
                       true -> list_unseen_events(Clock, History);
                       false -> maps:values(History)
                   end,
    update_events(Node, UnseenEvents),
    LastSeenClock = get_last_seen_event_clock(Clock,
                                              History),
    gen_server:cast(Node, {sync_from, Self, LastSeenClock}),
    {noreply, State};
handle_cast({sync_from, Node, Clock},
            State = #{history := History}) ->
    update_events(Node, list_unseen_events(Clock, History)),
    {noreply, State};
handle_cast({connect, Node},
            State = #{nodes := Nodes, history := History,
                      node := Self}) ->
    update_events(Node, maps:values(History)),
    {noreply,
     State#{clock := gen_server:call(Node, {join, Self}),
            nodes := add_node(Node, Self, Nodes)}};
handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Pid, Value) -> gen_server:cast(Pid, {add, Value}).

remove(Pid, Value) ->
    gen_server:cast(Pid, {remove, Value}).

connect(Pid, Node) ->
    gen_server:cast(Pid, {connect, Node}).

members(Pid) -> gen_server:call(Pid, members).

member(Pid, Value) ->
    gen_server:call(Pid, {member, Value}).

nodes(Pid) -> gen_server:call(Pid, nodes).

%%====================================================================
%% Internal functions
%%====================================================================

init_state(ServerRef) ->
    Clock = itc:seed(),
    Event = #event{action = init, clock = Clock,
                   value = none, node = ServerRef},
    #{history => #{{ServerRef, Clock} => Event},
      nodes => [], clock => Clock, node => ServerRef}.

sync_event(#event{clock = Clock},
           #{nodes := Nodes, node := Self}) ->
    Sync = fun (Pid) ->
                   gen_server:cast(Pid, {sync, Self, Clock})
           end,
    lists:foreach(Sync, Nodes).

handle_event(#event{action = init}, State) -> State;
handle_event(Event = #event{action = join,
                            value = [LeftNode, RightNode], clock = Clock},
             State = #{history := History, nodes := Nodes,
                       node := Self}) ->
    State#{history := add_event(Event, History),
           clock := Clock,
           nodes :=
               add_node(RightNode, Self,
                        add_node(LeftNode, Self, Nodes))};
handle_event(Event = #event{action = add,
                            clock = Clock},
             State = #{history := History}) ->
    State#{history := add_event(Event, History),
           clock := Clock};
handle_event(Event = #event{action = remove,
                            value = Removables, clock = Clock},
             State = #{history := History}) ->
    Clean = fun (K, _) -> not lists:member(K, Removables)
            end,
    CleanedHistory = maps:filter(Clean, History),
    State#{history := add_event(Event, CleanedHistory),
           clock := Clock}.

add_node(Node, Self, Nodes) ->
    if Node =:= Self -> Nodes;
       true -> lists:usort([Node | Nodes])
    end.

update_events(Node, Events) ->
    Update = fun (Event) ->
                     gen_server:cast(Node, {update, Event})
             end,
    lists:foreach(Update, Events).

add_event(Event, History) ->
    History#{get_event_key(Event) => Event}.

list_members(History) ->
    [E#event.value
     || E <- maps:values(History), E#event.action =:= add].

list_unseen_events(Clock, History) ->
    Filter = fun ({_, EventClock}, _) ->
                     not itc:leq(EventClock, Clock)
             end,
    maps:values(maps:filter(Filter, History)).

get_last_seen_event_clock(Clock, History) ->
    Clocks = [EventClock
              || {_, EventClock} <- maps:keys(History)],
    Filter = fun (EventClock) -> itc:leq(EventClock, Clock)
             end,
    Cmp = fun (C, D) -> itc:leq(D, C) end,
    case lists:filter(Filter, Clocks) of
        [] -> lists:last(History);
        [Last] -> Last;
        FilteredClocks ->
            erlang:hd(lists:sort(Cmp, FilteredClocks))
    end.

filter_event_by_value(Value, History) ->
    [K
     || {K, E} <- maps:to_list(History),
        E#event.value =:= Value].

get_event_key(#event{node = Node, clock = Clock}) ->
    {Node, Clock}.

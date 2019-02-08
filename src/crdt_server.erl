%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {history, nodes, clock}).

-record(event, {clock, action, value}).

%% Application callbacks
-export([add/2, connect/2, member/2, members/1, nodes/1,
         remove/2, start_link/0, start_link/1, stop/0, stop/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%%====================================================================
%% API
%%====================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          init_state(), []).

start_link() ->
    gen_server:start_link(?MODULE, init_state(), []).

stop(Pid) -> gen_server:call(Pid, stop).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------

handle_call(members, _From, State) ->
    {reply, lists:usort(list_members(State)), State};
handle_call({member, Value}, _From, State) ->
    {reply, lists:member(Value, list_members(State)),
     State};
handle_call(nodes, _From,
            State = #state{nodes = Nodes}) ->
    {reply, Nodes, State};
handle_call({join, NewNode}, From,
            State = #state{clock = Clock}) ->
    [LeftClock, RightClock] = itc:fork(Clock),
    gen_server:reply(From, RightClock),
    NewEvent = #event{clock = itc:event(LeftClock),
                      action = join, value = [NewNode, self()]},
    NewState = handle_event(NewEvent, State),
    update_events(NewNode, NewState#state.history),
    sync_event(NewEvent, NewState),
    {noreply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Value},
            State = #state{clock = Clock}) ->
    Event = #event{clock = itc:event(Clock), action = add,
                   value = Value},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({remove, Value},
            State = #state{clock = Clock}) ->
    Event = #event{clock = itc:event(Clock),
                   action = remove,
                   value = filter_event_itcs(Value, State)},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({update, Event},
            State = #state{clock = Clock}) ->
    NewState = handle_event(Event, State),
    NewClock = itc:event(itc:join(NewState#state.clock,
                                  Clock)),
    {noreply, NewState#state{clock = NewClock}};
handle_cast({sync, Clock, Node}, State) ->
    UnseenEvents = list_unseen_events(Clock, State),
    update_events(Node, UnseenEvents),
    #event{clock = LastSeenClock} =
        get_last_seen_event(UnseenEvents, State),
    gen_server:cast(Node,
                    {sync_from, LastSeenClock, self()}),
    {noreply, State};
handle_cast({sync_from, Clock, Node}, State) ->
    update_events(Node, list_unseen_events(Clock, State)),
    {noreply, State};
handle_cast({connect, Node},
            State = #state{nodes = Nodes, history = History}) ->
    update_events(Node, History),
    {noreply,
     State#state{clock =
                     gen_server:call(Node, {join, self()}),
                 nodes = add_node(Node, Nodes)}};
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

init_state() ->
    Clock = itc:seed(),
    Event = #event{action = init, clock = Clock,
                   value = none},
    #state{history = [Event], nodes = [], clock = Clock}.

list_members(#state{history = History}) ->
    [E#event.value || E <- History, E#event.action =:= add].

sync_event(#event{clock = Clock},
           #state{nodes = Nodes}) ->
    Sync = fun (Pid) ->
                   gen_server:cast(Pid, {sync, Clock, self()})
           end,
    lists:foreach(Sync, Nodes).

update_events(Node, History) ->
    Update = fun (Event) ->
                     gen_server:cast(Node, {update, Event})
             end,
    lists:foreach(Update, History).

list_unseen_events(Clock, #state{history = History}) ->
    Cmp = fun (E) -> not itc:leq(E#event.clock, Clock) end,
    lists:takewhile(Cmp, History).

get_last_seen_event(UnseenEvents,
                    #state{history = History}) ->
    Last = erlang:min(erlang:length(UnseenEvents) + 1,
                      erlang:length(History)),
    lists:nth(Last, History).

handle_event(#event{action = init}, State) -> State;
handle_event(Event = #event{action = join,
                            value = [LeftNode, RightNode], clock = Clock},
             State = #state{history = History, nodes = Nodes}) ->
    State#state{history = add_event(Event, History),
                clock = Clock,
                nodes = add_node(RightNode, add_node(LeftNode, Nodes))};
handle_event(Event = #event{action = add,
                            clock = Clock},
             State = #state{history = History}) ->
    State#state{history = add_event(Event, History),
                clock = Clock};
handle_event(Event = #event{action = remove,
                            value = Removables, clock = Clock},
             State = #state{history = History}) ->
    CleanedHistory = [E
                      || E <- History,
                         not lists:member(E#event.clock, Removables)],
    State#state{history = add_event(Event, CleanedHistory),
                clock = Clock}.

add_event(Event, History) ->
    Cmp = fun (E, F) ->
                  itc:leq(F#event.clock, E#event.clock)
          end,
    lists:usort(Cmp, [Event | History]).

add_node(Node, Nodes) ->
    if Node =:= self() -> Nodes;
       true -> lists:usort([Node | Nodes])
    end.

filter_event_itcs(Value, #state{history = History}) ->
    [E#event.clock
     || E <- History, E#event.value =:= Value].

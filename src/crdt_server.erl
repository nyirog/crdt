%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {history, nodes, itc}).

-record(event, {itc, action, value}).

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
            State = #state{itc = Itc}) ->
    [ItcLeft, ItcRight] = itc:fork(Itc),
    gen_server:reply(From, ItcRight),
    NewEvent = #event{itc = itc:event(ItcLeft),
                      action = join, value = [NewNode, self()]},
    NewState = handle_event(NewEvent, State),
    update_events(NewNode, NewState#state.history),
    sync_event(NewEvent, NewState),
    {noreply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Value}, State = #state{itc = Itc}) ->
    Event = #event{itc = itc:event(Itc), action = add,
                   value = Value},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({remove, Value},
            State = #state{itc = Itc}) ->
    Event = #event{itc = itc:event(Itc), action = remove,
                   value = filter_event_itcs(Value, State)},
    sync_event(Event, State),
    {noreply, handle_event(Event, State)};
handle_cast({update, Event},
            State = #state{itc = Itc}) ->
    NewState = handle_event(Event, State),
    NewItc = itc:event(itc:join(NewState#state.itc, Itc)),
    {noreply, NewState#state{itc = NewItc}};
handle_cast({sync, ItcEvent, Node}, State) ->
    UnseenEvents = list_unseen_events(ItcEvent, State),
    update_events(Node, UnseenEvents),
    #event{itc = LastSeenItc} =
        get_last_seen_event(UnseenEvents, State),
    gen_server:cast(Node, {sync_from, LastSeenItc, self()}),
    {noreply, State};
handle_cast({sync_from, ItcEvent, Node}, State) ->
    Events = list_unseen_events(ItcEvent, State),
    update_events(Node, Events),
    {noreply, State};
handle_cast({connect, Node},
            State = #state{nodes = Nodes, history = History}) ->
    update_events(Node, History),
    ItcEvent = gen_server:call(Node, {join, self()}),
    {noreply,
     State#state{itc = ItcEvent,
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
    Itc = itc:seed(),
    Event = #event{action = init, itc = Itc, value = none},
    #state{history = [Event], nodes = [], itc = Itc}.

list_members(#state{history = History}) ->
    [E#event.value || E <- History, E#event.action =:= add].

sync_event(#event{itc = Itc}, #state{nodes = Nodes}) ->
    lists:foreach(fun (Pid) ->
                          gen_server:cast(Pid, {sync, Itc, self()})
                  end,
                  Nodes).

update_events(Node, History) ->
    lists:foreach(fun (Event) ->
                          gen_server:cast(Node, {update, Event})
                  end,
                  History).

list_unseen_events(ItcEvent,
                   #state{history = History}) ->
    lists:takewhile(fun (#event{itc = Itc}) ->
                            not itc:leq(Itc, ItcEvent)
                    end,
                    History).

get_last_seen_event(UnseenEvents,
                    #state{history = History}) ->
    Last = erlang:min(erlang:length(UnseenEvents) + 1,
                      erlang:length(History)),
    lists:nth(Last, History).

handle_event(#event{action = init}, State) -> State;
handle_event(Event = #event{action = join,
                            value = [LeftNode, RightNode], itc = Itc},
             State = #state{history = History, nodes = Nodes}) ->
    State#state{history = add_event(Event, History),
                itc = Itc,
                nodes = add_node(RightNode, add_node(LeftNode, Nodes))};
handle_event(Event = #event{action = add, itc = Itc},
             State = #state{history = History}) ->
    State#state{history = add_event(Event, History),
                itc = Itc};
handle_event(Event = #event{action = remove,
                            value = Removables, itc = Itc},
             State = #state{history = History}) ->
    CleanedHistory = [E
                      || E <- History,
                         not lists:member(E#event.itc, Removables)],
    State#state{history = add_event(Event, CleanedHistory),
                itc = Itc}.

add_event(Event, History) ->
    lists:usort(fun (E, F) ->
                        itc:leq(F#event.itc, E#event.itc)
                end,
                [Event | History]).

add_node(Node, Nodes) ->
    if Node =:= self() -> Nodes;
       true -> lists:usort([Node | Nodes])
    end.

filter_event_itcs(Value, #state{history = History}) ->
    [E#event.itc || E <- History, E#event.value =:= Value].

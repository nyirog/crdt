%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {history, nodes, itc}).

-record(event, {itc, value}).

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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Value},
            State = #state{nodes = Nodes, itc = Itc}) ->
    ItcEvent = itc:event(Itc),
    lists:foreach(fun (Pid) ->
                          gen_server:cast(Pid, {add, ItcEvent, Value})
                  end,
                  Nodes),
    {noreply,
     State#state{history = add_event(ItcEvent, Value, State),
                 itc = ItcEvent}};
handle_cast({add, ItcEvent, Value},
            State = #state{itc = Itc}) ->
    {noreply,
     State#state{history = add_event(ItcEvent, Value, State),
                 itc = itc:event(itc:join(Itc, ItcEvent))}};
handle_cast({remove, Value},
            State = #state{history = History, nodes = Nodes,
                           itc = Itc}) ->
    ItcEvent = itc:event(Itc),
    lists:foreach(fun ({Pid, ItcDel}) ->
                          gen_server:cast(Pid, {delete, ItcDel, ItcEvent})
                  end,
                  [{Pid, E#event.itc}
                   || E <- History, Pid <- Nodes,
                      E#event.value =:= Value]),
    {noreply,
     State#state{history =
                     [E || E <- History, E#event.value =/= Value],
                 itc = ItcEvent}};
handle_cast({delete, ItcDel, ItcEvent},
            State = #state{history = History, itc = Itc}) ->
    {noreply,
     State#state{history =
                     [E || E <- History, E#event.itc =/= ItcDel],
                 itc = itc:event(itc:join(Itc, ItcEvent))}};
handle_cast({connect, Pid},
            State = #state{nodes = Nodes, itc = Itc}) ->
    NewNodes = lists:usort([Pid | Nodes]),
    [NewItc, ItcFork] = itc:fork(Itc),
    join(Pid, ItcFork, lists:usort([self() | NewNodes])),
    {noreply, State#state{nodes = NewNodes, itc = NewItc}};
handle_cast({join, ItcFork, OtherNodes},
            State = #state{nodes = Nodes, itc = Itc}) ->
    NormalizedNodes = lists:delete(self(), OtherNodes),
    if NormalizedNodes =:= Nodes -> {noreply, State};
       true ->
           NewNodes = lists:umerge(NormalizedNodes, Nodes),
           AllNodes = lists:usort([self() | NewNodes]),
           lists:foreach(fun (Pid) -> join(Pid, ItcFork, AllNodes)
                         end,
                         NewNodes),
           {noreply,
            State#state{nodes = NewNodes,
                        itc = itc:event(itc:join(Itc, ItcFork))}}
    end;
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
    #state{history = [], nodes = [], itc = itc:seed()}.

join(Pid, Itc, Nodes) ->
    gen_server:cast(Pid, {join, Itc, Nodes}).

list_members(#state{history = History}) ->
    [E#event.value || E <- History].

add_event(Itc, Value, #state{history = History}) ->
    lists:usort(fun (E, F) ->
                        itc:leq(F#event.itc, E#event.itc)
                end,
                [#event{itc = Itc, value = Value} | History]).

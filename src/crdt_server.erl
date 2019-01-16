%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {entries, nodes, itc}).

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

handle_call(members, _From,
            State = #state{entries = Entries}) ->
    {reply,
     sets:to_list(sets:from_list(maps:values(Entries))),
     State};
handle_call({member, Key}, _From,
            State = #state{entries = Entries}) ->
    {reply, lists:member(Key, maps:values(Entries)), State};
handle_call(nodes, _From,
            State = #state{nodes = Nodes}) ->
    {reply, sets:to_list(Nodes), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Key},
            State = #state{entries = Entries, nodes = Nodes,
                           itc = Itc}) ->
    ItcAdd = itc:event(Itc),
    lists:foreach(fun (Pid) ->
                          gen_server:cast(Pid, {add, ItcAdd, Key})
                  end,
                  sets:to_list(Nodes)),
    {noreply,
     State#state{entries = Entries#{ItcAdd => Key},
                 itc = ItcAdd}};
handle_cast({add, ItcAdd, Key},
            State = #state{entries = Entries, itc = Itc}) ->
    {noreply,
     State#state{entries = Entries#{ItcAdd => Key},
                 itc = itc:event(itc:join(Itc, ItcAdd))}};
handle_cast({remove, Key},
            State = #state{entries = Entries, nodes = Nodes,
                           itc = Itc}) ->
    RemovableElements = maps:filter(fun (_Id, EntryKey) ->
                                            EntryKey =:= Key
                                    end,
                                    Entries),
    RemovableTasks = [{Pid, Id}
                      || {Id, _Key} <- maps:to_list(RemovableElements),
                         Pid <- sets:to_list(Nodes)],
    ItcDel = itc:event(Itc),
    lists:foreach(fun ({Pid, Id}) ->
                          gen_server:cast(Pid, {delete, Id, ItcDel})
                  end,
                  RemovableTasks),
    NewEntries = maps:filter(fun (_Id, EntryKey) ->
                                     EntryKey =/= Key
                             end,
                             Entries),
    {noreply,
     State#state{entries = NewEntries, itc = ItcDel}};
handle_cast({delete, Id, ItcDel},
            State = #state{entries = Entries, itc = Itc}) ->
    {noreply,
     State#state{entries = maps:remove(Id, Entries),
                 itc = itc:event(itc:join(Itc, ItcDel))}};
handle_cast({connect, Pid},
            State = #state{nodes = Nodes, itc = Itc}) ->
    NewNodes = sets:add_element(Pid, Nodes),
    [NewItc, ItcFork] = itc:fork(Itc),
    join(Pid, ItcFork, sets:add_element(self(), NewNodes)),
    {noreply, State#state{nodes = NewNodes, itc = NewItc}};
handle_cast({join, ItcFork, OtherNodes},
            State = #state{nodes = Nodes, itc = Itc}) ->
    NormalizedNodes = sets:del_element(self(), OtherNodes),
    if NormalizedNodes =:= Nodes -> {noreply, State};
       true ->
           NewNodes = sets:union(NormalizedNodes, Nodes),
           AllNodes = sets:add_element(self(), NewNodes),
           lists:foreach(fun (Pid) -> join(Pid, ItcFork, AllNodes)
                         end,
                         sets:to_list(NewNodes)),
           {noreply,
            State#state{nodes = NewNodes,
                        itc = itc:event(itc:join(Itc, ItcFork))}}
    end;
handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Pid, Key) -> gen_server:cast(Pid, {add, Key}).

remove(Pid, Key) -> gen_server:cast(Pid, {remove, Key}).

connect(Pid, Node) ->
    gen_server:cast(Pid, {connect, Node}).

members(Pid) -> gen_server:call(Pid, members).

member(Pid, Key) -> gen_server:call(Pid, {member, Key}).

nodes(Pid) -> gen_server:call(Pid, nodes).

%%====================================================================
%% Internal functions
%%====================================================================

init_state() ->
    #state{entries = maps:new(), nodes = sets:new(),
           itc = itc:seed()}.

join(Pid, Itc, Nodes) ->
    gen_server:cast(Pid, {join, Itc, Nodes}).

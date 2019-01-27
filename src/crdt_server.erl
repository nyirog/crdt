%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {members, nodes, itc}).

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
handle_call({member, Member}, _From, State) ->
    {reply, lists:member(Member, list_members(State)),
     State};
handle_call(nodes, _From,
            State = #state{nodes = Nodes}) ->
    {reply, Nodes, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Member},
            State = #state{members = Members, nodes = Nodes,
                           itc = Itc}) ->
    ItcAdd = itc:event(Itc),
    lists:foreach(fun (Pid) ->
                          gen_server:cast(Pid, {add, ItcAdd, Member})
                  end,
                  Nodes),
    {noreply,
     State#state{members = [{ItcAdd, Member} | Members],
                 itc = ItcAdd}};
handle_cast({add, ItcAdd, Member},
            State = #state{members = Members, itc = Itc}) ->
    {noreply,
     State#state{members = [{ItcAdd, Member} | Members],
                 itc = itc:event(itc:join(Itc, ItcAdd))}};
handle_cast({remove, Member},
            State = #state{members = Members, nodes = Nodes,
                           itc = Itc}) ->
    RemovableElements = [V1
                         || V1 <- Members, value_eq(V1, Member)],
    RemovableTasks = [{Pid, Id}
                      || {Id, _Member} <- RemovableElements, Pid <- Nodes],
    ItcDel = itc:event(Itc),
    lists:foreach(fun ({Pid, Id}) ->
                          gen_server:cast(Pid, {delete, Id, ItcDel})
                  end,
                  RemovableTasks),
    NewMembers = [V2
                  || V2 <- Members, value_neq(V2, Member)],
    {noreply,
     State#state{members = NewMembers, itc = ItcDel}};
handle_cast({delete, Id, ItcDel},
            State = #state{members = Members, itc = Itc}) ->
    {noreply,
     State#state{members =
                     [V3 || V3 <- Members, id_neq(V3, Id)],
                 itc = itc:event(itc:join(Itc, ItcDel))}};
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

value_eq({_Id, Value}, Member) -> Value =:= Member.

value_neq({_Id, Value}, Member) -> Value =/= Member.

id_neq({MemberId, _Value}, Id) -> Id =/= MemberId.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Pid, Member) -> gen_server:cast(Pid, {add, Member}).

remove(Pid, Member) ->
    gen_server:cast(Pid, {remove, Member}).

connect(Pid, Node) ->
    gen_server:cast(Pid, {connect, Node}).

members(Pid) -> gen_server:call(Pid, members).

member(Pid, Member) ->
    gen_server:call(Pid, {member, Member}).

nodes(Pid) -> gen_server:call(Pid, nodes).

%%====================================================================
%% Internal functions
%%====================================================================

init_state() ->
    #state{members = [], nodes = [], itc = itc:seed()}.

join(Pid, Itc, Nodes) ->
    gen_server:cast(Pid, {join, Itc, Nodes}).

list_members(#state{members = Members}) ->
    [Value || {_Id, Value} <- Members].

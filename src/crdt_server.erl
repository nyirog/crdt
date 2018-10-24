%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {clock, entries, nodes}).

%% Application callbacks
-export([add/2, connect/2, member/2, members/1, nodes/1, remove/2, start_link/0,
         stop/0]).

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    quickrand:seed(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(), []).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

%%--------------------------------------------------------------------

handle_call(members, _From, State = #state{entries = Entries}) ->
    {reply, sets:from_list(maps:values(Entries)), State};
handle_call({member, Key}, _From, State = #state{entries = Entries}) ->
    {reply, lists:member(Key, maps:values(Entries)), State};
handle_call(nodes, _From, State = #state{nodes = Nodes}) ->
    {reply, sets:to_list(Nodes), State};
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call({join, OtherNodes}, {Pid, _Tag}, State = #state{nodes = Nodes}) ->
    RemoteNodes = sets:del_element(self(), sets:add_element(Pid, OtherNodes)),
    {reply, Nodes, State#state{nodes = sets:union(Nodes, RemoteNodes)}};
handle_call({connect, RemotePid}, _From, State = #state{nodes = Nodes}) ->
    RemoteNodes = sets:del_element(self(),
                                   sets:add_element(RemotePid,
                                                    gen_server:call(RemotePid, {join, Nodes}))),
    {reply, RemoteNodes, State#state{nodes = sets:union(Nodes, RemoteNodes)}};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Key}, State = #state{clock = Clock, entries = Entries}) ->
    NewState = State#state{clock = Clock + 1,
                           entries = Entries#{create_id() => Key}},
    {noreply, NewState};
handle_cast({remove, Key}, State = #state{entries = Entries}) ->
    NewEntries = maps:filter(fun (_Id, EntryKey) -> EntryKey =/= Key end, Entries),
    {noreply, State#state{entries = NewEntries}};
handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Pid, Key) -> gen_server:cast(Pid, {add, Key}).

remove(Pid, Key) -> gen_server:cast(Pid, {remove, Key}).

connect(Pid, Node) -> gen_server:call(Pid, {connect, Node}).

members(Pid) -> gen_server:call(Pid, members).

member(Pid, Key) -> gen_server:call(Pid, {member, Key}).

nodes(Pid) -> gen_server:call(Pid, nodes).

%%====================================================================
%% Internal functions
%%====================================================================

create_id() -> uuid:new(self()).

init_state() -> #state{clock = 0, entries = maps:new(), nodes = sets:new()}.

%%%-------------------------------------------------------------------
%% @doc crdt server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {clock, entries}).

%% Application callbacks
-export([add/1, connect/1, member/1, members/0, remove/1, start_link/0,
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
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call({connect, Node}, _From, State) ->
    Nodes = crdt_cluster:connect(Node), {reply, Nodes, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Key}, _State = #state{clock = Clock, entries = Entries}) ->
    NewState = #state{clock = Clock + 1, entries = Entries#{create_id() => Key}},
    {noreply, NewState};
handle_cast({remove, Key}, _State = #state{clock = Clock, entries = Entries}) ->
    NewEntries = maps:filter(fun (_Id, EntryKey) -> EntryKey =/= Key end, Entries),
    {noreply, #state{clock = Clock, entries = NewEntries}};
handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Key) -> gen_server:cast(?MODULE, {add, Key}).

remove(Key) -> gen_server:cast(?MODULE, {remove, Key}).

connect(Node) -> gen_server:call(?MODULE, {connect, Node}).

members() -> gen_server:call(?MODULE, members).

member(Key) -> gen_server:call(?MODULE, {member, Key}).

%%====================================================================
%% Internal functions
%%====================================================================

create_id() -> uuid:new(self()).

init_state() -> #state{clock = 0, entries = maps:new()}.

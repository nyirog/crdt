%%%-------------------------------------------------------------------
%% @doc crdt public server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {clock, entries}).

%% Application callbacks
-export([add/1, list/0, member/1, remove/1, start_link/0, stop/0]).

-export([handle_call/3, handle_cast/2, init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    quickrand:seed(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(), []).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

%%--------------------------------------------------------------------

handle_call(list, _From, State = #state{entries = Entries}) ->
    VisibleEntries = sets:from_list([Key || {_Id, Key} <- sets:to_list(Entries)]),
    {reply, sets:to_list(VisibleEntries), State};
handle_call({member, Key}, _From, State = #state{entries = Entries}) ->
    KeyEntries = sets:filter(fun ({_Id, EntryKey}) -> EntryKey =:= Key end,
                             Entries),
    {reply, sets:size(KeyEntries) =/= 0, State};
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast({add, Key}, _State = #state{clock = Clock, entries = Entries}) ->
    NewState = #state{clock = Clock + 1,
                      entries = sets:add_element({create_id(), Key}, Entries)},
    {noreply, NewState};
handle_cast({remove, Key}, _State = #state{clock = Clock, entries = Entries}) ->
    NewEntries = sets:filter(fun ({_Id, EntryKey}) -> EntryKey =/= Key end,
                             Entries),
    {noreply, #state{clock = Clock, entries = NewEntries}};
handle_cast(_Event, State) -> {noreply, State}.

%%--------------------------------------------------------------------

add(Key) -> gen_server:cast(?MODULE, {add, Key}).

remove(Key) -> gen_server:cast(?MODULE, {remove, Key}).

list() -> gen_server:call(?MODULE, list).

member(Key) -> gen_server:call(?MODULE, {member, Key}).

%%====================================================================
%% Internal functions
%%====================================================================

create_id() -> uuid:new(self()).

init_state() -> #state{clock = 0, entries = sets:new()}.

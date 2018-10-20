%%%-------------------------------------------------------------------
%% @doc crdt public server
%% @end
%%%-------------------------------------------------------------------

-module(crdt_server).

-behaviour(gen_server).

-record(state, {clock, entries}).

%% Application callbacks
-export([add/1, list/0, start_link/0]).

-export([handle_call/3, handle_cast/2, init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(), []).

init(State) -> {ok, State}.

%%--------------------------------------------------------------------

handle_call(list, _From, State = #state{entries = Entries}) ->
    VisibleEntries = sets:from_list([Key||{_Id, Key} <- sets:to_list(Entries)]),
    {reply, sets:to_list(VisibleEntries), State}.

handle_cast({add, Key}, State = #state{clock=Clock, entries=Entries}) ->
    NewState = #state{clock=Clock+1, entries=sets:add_element({create_id(State), Key}, Entries)},
    {noreply, NewState}.

%%--------------------------------------------------------------------

add(Key) -> gen_server:cast(?MODULE, {add, Key}).

list() -> gen_server:call(?MODULE, list).

%%====================================================================
%% Internal functions
%%====================================================================

create_id(_State = #state{clock=Clock}) -> Clock.

init_state() -> #state{clock=0, entries=sets:new()}.

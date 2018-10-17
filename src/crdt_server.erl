-module(crdt_server).

-behaviour(gen_server).

-export([add/1, list/0, start_link/0]).

-export([handle_call/3, handle_cast/2, init/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(State) -> {ok, State}.

handle_call(list, _From, State) -> {reply, State, State}.

handle_cast({add, Key}, State) -> {noreply, [Key | State]}.

add(Key) -> gen_server:cast(?MODULE, {add, Key}).

list() -> gen_server:call(?MODULE, list).

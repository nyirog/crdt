-module(crdt_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([handle_call/3, handle_cast/2, init/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init(State) -> {ok, State}.

handle_call(_Request, _From, State) -> {reply, State}.

handle_cast(_Message, State) -> {noreply, State}.

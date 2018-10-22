%%%-------------------------------------------------------------------
%% @doc crdt cluster
%% @end
%%%-------------------------------------------------------------------

-module(crdt_cluster).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================


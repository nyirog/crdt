%%%-------------------------------------------------------------------
%% @doc crdt cluster
%% @end
%%%-------------------------------------------------------------------

-module(crdt_cluster).

-behaviour(gen_server).

-record(state, {nodes}).

%% Application callbacks
-export([connect/1, nodes/0, start_link/0, stop/0]).

-export([handle_call/3, handle_cast/2, handle_info/2, init/1]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(), []).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

%%--------------------------------------------------------------------

handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call(nodes, _From, State = #state{nodes = Nodes}) ->
    {reply, Nodes, State};
handle_call(connect, {Pid, Tag}, _State = #state{nodes = Nodes}) ->
    {reply, Nodes, #state{nodes = [Pid | Nodes]}};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------

connect(Node) -> gen_server:call({?MODULE, Node}, connect).

nodes() -> gen_server:call(?MODULE, nodes).

%%====================================================================
%% Internal functions
%%====================================================================

init_state() -> #state{nodes = []}.

%%%-------------------------------------------------------------------
%% @doc crdt public API
%% @end
%%%-------------------------------------------------------------------

-module(crdt).

-behaviour(application).

-define(SERVER, crdt_server).

%% Application callbacks
-export([start/2, stop/1]).

-export([add/1, connect/1, member/1, members/0, nodes/0,
         remove/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, []) ->
    crdt_sup:start_link({?SERVER, erlang:node()}).

stop(_State) -> crdt_sup:stop().

%%--------------------------------------------------------------------

add(Key) -> crdt_server:add(?SERVER, Key).

remove(Key) -> crdt_server:remove(?SERVER, Key).

members() -> crdt_server:members(?SERVER).

member(Key) -> crdt_server:member(?SERVER, Key).

nodes() -> crdt_server:nodes(?SERVER).

connect(Node) ->
    crdt_server:connect(?SERVER, {?SERVER, Node}).

%%====================================================================
%% Internal functions
%%====================================================================


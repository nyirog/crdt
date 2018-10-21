%%%-------------------------------------------------------------------
%% @doc crdt public API
%% @end
%%%-------------------------------------------------------------------

-module(crdt).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([add/1, member/1, members/0, remove/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, []) -> crdt_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) -> crdt_sup:stop().

add(Key) -> crdt_server:add(Key).

remove(Key) -> crdt_server:remove(Key).

members() -> crdt_server:members().

member(Key) -> crdt_server:member(Key).

%%====================================================================
%% Internal functions
%%====================================================================


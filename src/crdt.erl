%%%-------------------------------------------------------------------
%% @doc crdt public API
%% @end
%%%-------------------------------------------------------------------

-module(crdt).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([add/1, connect/1, member/1, members/0, nodes/0, remove/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, []) -> crdt_sup:start_link().

stop(_State) -> crdt_sup:stop().

%%--------------------------------------------------------------------

add(Key) -> crdt_server:add(crdt_server, Key).

remove(Key) -> crdt_server:remove(crdt_server, Key).

members() -> crdt_server:members(crdt_server).

member(Key) -> crdt_server:member(crdt_server, Key).

nodes() -> crdt_server:nodes(crdt_server).

connect(Node) ->
    case rpc:call(Node, erlang, whereis, [crdt_server]) of
        Pid when erlang:is_pid(Pid) -> crdt_server:connect(crdt_server, Pid);
        _ -> undefined
    end.

%%====================================================================
%% Internal functions
%%====================================================================


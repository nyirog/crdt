%%%-------------------------------------------------------------------
%% @doc crdt public API
%% @end
%%%-------------------------------------------------------------------

-module(crdt).

-behaviour(application).

-define(SERVER, crdt_server).

%% Application callbacks
-export([install/0, install/2, start/2, stop/1]).

-export([add/1, connect/1, member/1, members/0, nodes/0,
         remove/1]).

-include("crdt_server.hrl").

%%====================================================================
%% API
%%====================================================================

start(normal, []) ->
    ok = mnesia:wait_for_tables([?SERVER], 5000),
    crdt_sup:start_link({?SERVER, erlang:node()}).

stop(_State) -> crdt_sup:stop().

install() -> install([node()], [?SERVER]).

install(Nodes, Names) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    Create = fun (Name) ->
                     mnesia:create_table(Name,
                                         [{attributes,
                                           record_info(fields, event)},
                                          {record_name, event}, {type, set},
                                          {disc_copies, Nodes}])
             end,
    lists:foreach(Create, Names).

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


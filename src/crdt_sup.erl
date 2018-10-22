%%%-------------------------------------------------------------------
%% @doc crdt top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(crdt_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() -> supervisor:start_link({local, crdt}, ?MODULE, []).

stop() ->
    case whereis(crdt) of
        P when is_pid(P) -> exit(P, kill);
        _ -> ok
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok,
     {{one_for_all, 0, 1},
      [{server, {crdt_server, start_link, []}, permanent, 5000, worker,
        [crdt_server]},
       {cluster, {crdt_cluster, start_link, []}, permanent, 5000, worker,
        [crdt_cluster]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================


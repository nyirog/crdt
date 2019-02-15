-module(crdt_trc).

-export([start/0, stop/1, trc/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok, Viewer} = et_viewer:start([]),
    Collector = et_viewer:get_collector_pid(Viewer),
    dbg:tracer(process, {fun trace/2, Collector}),
    dbg:tp(gen_server, cast, []),
    dbg:tp(gen_server, call, []),
    {ok, Viewer}.

stop(Viewer) -> et_viewer:stop(Viewer).

trc(PidOrName) -> dbg:p(PidOrName, call).

%%====================================================================
%% Internal functions
%%====================================================================

trace(end_of_trace, Collector) -> Collector;
trace({trace, Pid, call,
       {gen_server, _, [To, Message]}},
      Collector) ->
    et_collector:report_event(Collector, 80, Pid,
                              erlang:whereis(To), message_to_label(Message),
                              Message),
    Collector.

message_to_label({update, Event}) ->
    Label = io_lib:format("update: ~p",
                          [erlang:element(2, Event)]),
    lists:flatten(Label);
message_to_label({sync, Node, {_Id, Event}}) ->
    Label = io_lib:format("sync ~p @ ~p", [Event, Node]),
    lists:flatten(Label);
message_to_label({sync_from, Node, {_Id, Event}}) ->
    Label = io_lib:format("sync from ~p @ ~p",
                          [Event, Node]),
    lists:flatten(Label);
message_to_label({connect, Node}) ->
    Label = io_lib:format("connect: ~p", [Node]),
    lists:flatten(Label);
message_to_label({add, Value}) ->
    Label = io_lib:format("add: ~p", [Value]),
    lists:flatten(Label);
message_to_label({remove, Value}) ->
    Label = io_lib:format("remove: ~p", [Value]),
    lists:flatten(Label);
message_to_label(Message) -> erlang:element(1, Message).

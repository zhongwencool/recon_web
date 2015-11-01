-module(recon_web).
-export([start/0, start/1]).
-export([top/0, top/1]).

-spec start() -> ok.
start() ->
    application:ensure_all_started(recon_web).

-spec start([debug]) -> ok.
start([debug]) ->
    application:ensure_all_started(recon_web),
    lager:set_loglevel(lager_console_backend, debug).

%% @doc a top tool in erlang shell the reflushtime is Milliseconds
-define(TOP_MIN_REFLUSH_INTERAL, 2000).
-spec top() -> stop.
top() -> top(?TOP_MIN_REFLUSH_INTERAL).
-spec top(pos_integer()) -> stop.
top(ReflushMillSecond)when ReflushMillSecond >= ?TOP_MIN_REFLUSH_INTERAL ->
  Pid = spawn_link(fun() -> recon_web_top:loop(ReflushMillSecond) end),
  recon_web_top:top(Pid).


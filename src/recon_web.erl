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

-spec top() -> stop.
top() -> top(2000).
-spec top(pos_integer()) -> stop.
top(ReflushTime) ->
  Pid = spawn_link(fun() -> recon_web_top:loop(ReflushTime) end),
  recon_web_top:top(Pid).

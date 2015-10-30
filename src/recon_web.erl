-module(recon_web).
-export([start/0, start/1]).
-export([top/0]).

-spec start() -> ok.
start() ->
    application:ensure_all_started(recon_web).

-spec start([debug]) -> ok.
start([debug]) ->
    application:ensure_all_started(recon_web),
    lager:set_loglevel(lager_console_backend, debug).

top() ->
  Pid = spawn_link(fun() -> loop() end),
  top1(Pid).

top1(Pid) ->
  Input = io:get_line("Input q to Quit>"),
  io:format("~p~n", [Input]),
  case  Input of
    "q\n" -> erlang:send(Pid, stop);
    _ -> top1(Pid)
  end.

loop() ->
  io:format("\e[H\e[J"),
  recon_web_top:system_status(),
  recon_web_top:recon_status(),
  erlang:send_after(2000, self(), loop),
  receive
    stop -> stop;
    loop -> loop()
  end.

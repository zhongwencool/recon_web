-module(recon_web).
 

-export([start/0,start/1]).

start() ->
    application:ensure_all_started(recon_web). 

start([debug]) ->
    application:ensure_all_started(recon_web),
    lager:set_loglevel(lager_console_backend, debug).



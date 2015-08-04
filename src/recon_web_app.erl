-module(recon_web_app).

-behaviour(application).
-include("recon_web.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, recon_web, "index.html", [{mimetypes, cow_mimetypes, all}]}},
      {"/static/[...]", cowboy_static, {priv_dir, recon_web, "static/"}},
      {"/vendor/[...]", cowboy_static, {priv_dir, recon_web, "vendor/"}},
      {"/js/[...]", cowboy_static, {priv_dir, recon_web, "js/"}},
      {"/socket.io/1/[...]", recon_web_handle, [
        recon_web_session:configure([{heartbeat, 6000},
          {heartbeat_timeout, 30000},
          {session_timeout, 30000},
          {protocol, recon_web_protocol}])
    ]}]}]),
  {ok, Port} = application:get_env(port),
  NetWorks = case application:get_env(ip) of
               {ok, IP} -> [{port, Port}, {ip, IP}];
               _ -> [{port, Port}]
             end,
  {ok, _T} = cowboy:start_http(http, 100, NetWorks,
    [{env, [{dispatch, Dispatch}]}]),
  lager:debug("Dispatch ok:~p~n", [{?MODULE, Dispatch}]),
  ?SESSION_MANAGER_ETS = ets:new(?SESSION_MANAGER_ETS, [public, named_table]),
  recon_web_sup:start_link().

stop(_State) ->
  ok.

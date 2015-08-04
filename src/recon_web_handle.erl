%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(recon_web_handle).
-include("recon_web.hrl").

%%API
-export([
  notify_handler_have_new_message/2]).

%%CALLBACK
-export([init/3,
  info/3,
  terminate/3,
  handle/2,
  websocket_init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3]).


-record(http_state, {action, config = #config{},
  session_id, heartbeat_tref, pid = undefined}).

%% @doc session tell handler has new message in session
notify_handler_have_new_message(HanderPid, SessionPid) ->
  erlang:send(HanderPid, {message_arrived, SessionPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  handlers callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({_, http}, Req, [Config]) ->
  {Method, _} = cowboy_req:method(Req),
  {PathInfo, _} = cowboy_req:path_info(Req),
  lager:debug("+++++ PathInfo:~p~n", [PathInfo]),
  init_by_method(Method, PathInfo, Config, Req).

%%1) http://127.0.0.1:8080/socket.io/1/?t=1436608179209
init_by_method(_Method, [] = _PathInfo,
    Config = #config{heartbeat_timeout = HeartbeatTimeout, session_timeout = SessionTimeout, opts = Opts},
    Req) ->
  Sid = recon_web_uuids:new(),
  HeartbeatTimeoutBin = list_to_binary(integer_to_list(HeartbeatTimeout div 1000)),
  SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout div 1000)),

  {ok, _SessionPid} = recon_web_session:create(Sid, SessionTimeout, Opts),
  Result = << ":", HeartbeatTimeoutBin/binary, ":", SessionTimeoutBin/binary, ":websocket, xhr-polling">>,
  {ok, Req1} = cowboy_req:reply(200, ?TEXT_HEAD, <<Sid/binary, Result/binary>>, Req),
  {ok, Req1, #http_state{action = ?CREATE_SESSION, config = Config}};

%%2) ws://127.0.0.1:8080/socket.io/1/websocket/8080fa8492eb609e79471f1c5e396681 GET
init_by_method(_Method, [<<"websocket">>, _Sid], Config, Req) ->
  {upgrade, protocol, cowboy_websocket, Req, {Config}};

%%3) ws://127.0.0.1:8080/socket.io/xhr-polling/8080fa8492eb609e79471f1c5e396681 GET
init_by_method(<<"GET">>, [<<"xhr-polling">>, Sid], Config = #config{protocol = Protocol}, Req) ->
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      case recon_web_session:set_caller_and_pull_msg(Pid, self()) of
        ?SESSION_IN_USE ->
          {ok, Req, #http_state{action = ?SESSION_IN_USE, config = Config, session_id = Sid}};
        [] ->
          TRef = erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
          {cowboy_loop, Req, #http_state{action = ?HEARTBEAT, config = Config, session_id = Sid,
            heartbeat_tref = TRef, pid = Pid}, infinity};
        %%{ok, Req, #http_state{action = ?DATA, config = Config, session_id = Sid, pid = Pid}};
        Messages ->
          Req1 = cowboy_req:reply(200, ?TEXT_HEAD, Protocol:encode(Messages), Req),
          {ok, Req1, #http_state{action = ?DATA, config = Config, session_id = Sid, pid = Pid}}
      end;
    {error, not_found} ->
      {ok, Req, #http_state{action =?SESSION_NOT_FIND, session_id = Sid, config = Config}}
  end;

%% @todo support POST sometime client will send post .... bug!!!
init_by_method(<<"POST">>, [<<"xhr-polling">>, Sid], Config=#config{protocol = Protocol}, Req) ->
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      case cowboy_req:body(Req) of
        {ok, Body, Req1} ->
          Messages = Protocol:decode(Body),
          recon_web_session:deliver_msg(Pid, Messages),
          {ok, Req1, #http_state{action = ?OK, config = Config, session_id = Sid}};
        {error, _} ->
          {shutdown, Req, #http_state{action =?WRONG_ACTION, config = Config, session_id = Sid}}
      end;
    {error, not_found} ->
      {ok, Req, #http_state{action =?SESSION_NOT_FIND, session_id = Sid, config = Config}}
  end;

init_by_method(_Method, _PathInfo, Config, Req) ->
  {ok, Req, #http_state{config = Config}}.

%% @todo no useness  del
info({timeout, TRef, {?MODULE, _Pid}}, Req, HttpState =
  #http_state{action = ?HEARTBEAT, heartbeat_tref = TRef}) ->
  lager:error("recon_web_handler1:info~p~n", [timeout]),
  {loop, Req, HttpState#http_state{heartbeat_tref = undefined}};

info(Info, Req, HttpState) ->
  lager:error("recon_web_handler:info~p~n", [Info]),
  {ok, Req, HttpState}.

terminate(_Reason, _Req, _HttpState = #http_state{action = Action}) when
  Action == ?CREATE_SESSION;Action == ?SESSION_IN_USE->
  ok;
terminate(_Reason, _Req, _HttpState = #http_state{heartbeat_tref = HeartbeatTRef, pid = Pid}) ->
  safe_unlink_caller(Pid, self()),
  case HeartbeatTRef of
    undefined -> ok;
    _ -> erlang:cancel_timer(HeartbeatTRef)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% Http handlers callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(Req, HttpState = #http_state{action = ?CREATE_SESSION}) ->
  {ok, Req, HttpState};

handle(Req, HttpState = #http_state{action = ?SESSION_NOT_FIND}) ->
  {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req1, HttpState};

handle(Req, HttpState = #http_state{action = ?SESSION_IN_USE}) ->
  {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req1, HttpState};

handle(Req, HttpState = #http_state{action = ?OK}) ->
  {ok, Req1} = cowboy_req:reply(200, ?TEXT_HEAD, <<>>, Req),
  {ok, Req1, HttpState};

handle(Req, HttpState) ->
  {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
  {ok, Req1, HttpState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Websocket handlers callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% when upgrade proto to websocket
websocket_init(_TransportName, Req, {Config}) ->
  {PathInfo, _} = cowboy_req:path_info(Req),
  [<<"websocket">>, _Sid] = PathInfo,
  self() ! post_init,
  {ok, Req, {Config}}.

websocket_info(post_init, Req, {Config}) ->
  {[<<"websocket">>, Sid], _Req1} = cowboy_req:path_info(Req),
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      self() ! {go, Sid},
      erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
      {ok, Req, {Config, Pid}};
    {error, not_found} -> {shutdown, {Config, undefined}}
  end;

websocket_info({go, Sid}, Req, {Config, Pid}) ->
  case recon_web_session:set_caller_and_pull_msg(Pid, self()) of
    session_in_use -> {ok, Req, {Config, Pid}};
    Messages ->
      notify_when_first_connect(Messages, Pid, Sid),
      reply_ws_messages(Req, Messages, {Config, Pid})
  end;

websocket_info({timeout, _TRef, {?MODULE, Pid}}, Req, {Config = #config{protocol = Protocol}, Pid}) ->
  recon_web_session:refresh(Pid),
  erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
  {reply, {text, Protocol:encode(heartbeat)}, Req, {Config, Pid}};

websocket_info({message_arrived, Pid}, Req, State) ->
  Messages =  recon_web_session:pull_msg_from_session(Pid),
  reply_ws_messages(Req, Messages, State);

%%heartbeat
websocket_info({timeout, _TRef, {?MODULE, Pid}}, Req, State =
  {#config{protocol = Protocol, heartbeat = Heartbeat}, Pid}) ->
  recon_web_session:refresh(Pid),
  erlang:start_timer(Heartbeat, self(), {?MODULE, Pid}),
  Packet = Protocol:encode(heartbeat),
  {reply, {text, Packet}, Req, State};

%% session process DOWN because we monitor before
websocket_info({'DOWN', _Ref, process, Pid, _Reason}, Req, State = {_Config, Pid}) ->
  {shutdown, Req, State};

websocket_info(Info, Req, State) ->
  lager:info("unknow wesocket_info~p~n", [?MODULE, Info]),
  {ok, Req, State}.

%% message from client websocket
websocket_handle({text, Data}, Req, State = {#config{protocol = Protocol}, Pid}) ->
  Messages = Protocol:decode(Data),
  lager:debug("from client text~p~n", [Messages]),
  recon_web_session:deliver_msg(Pid, Messages),
  {ok, Req, State};

websocket_handle(Data, Req, State) ->
  lager:error("unknow send from client~p~n", [Data]),
  {ok, Req, State}.

websocket_terminate(Reason, _Req, {_Config, Pid}) ->
  lager:info("recon_web_handler1 terminate: Reason~p~n", [Reason]),
  recon_web_session:disconnect(Pid),
  ok;
websocket_terminate(Reason, _Req, State) ->
  lager:info("recon_web_handler2 terminate: Reason~p~n,State~p~n", [Reason, State]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_unlink_caller(Pid, Caller) ->
  case Pid =/= undefined andalso is_process_alive(Pid) of
    true -> recon_web_session:unsub_caller(Pid, Caller);
    false -> ok
  end.

notify_when_first_connect(Messages, SessionPid, Sid) ->
  case lists:keyfind(connect, 1, Messages) of
    {connect, <<>>} ->  recon_server:request_system_info(SessionPid, Sid);
    _ -> ok
  end.

reply_ws_messages(Req, Messages, State = {#config{protocol = Protocol}, _Pid}) ->
  case Protocol:encode(Messages) of
    <<>> -> {ok, Req, State};
    Packet -> {reply, {text, Packet}, Req, State}
  end.
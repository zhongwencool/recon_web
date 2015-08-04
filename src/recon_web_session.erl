-module(recon_web_session).
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

-behaviour(gen_server).

-include("recon_web.hrl").
%% API
-export([start_link/3, configure/1]).
-export([create/3,
  find/1,
  set_caller_and_pull_msg/2,
  pull_msg_from_session/1,

  deliver_msg/2,
  send_message/3,
  send_obj/3,
  refresh/1,
  disconnect/1,
  unsub_caller/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%%for test
-export([
        send_message_to_client/2
        ]).

-record(state, {id,
  session_timeout,
  session_timeout_tref,
  caller,
  registered = false,
  opts,
  messages = []
  }).

%%%===================================================================
%%% API
%%%===================================================================
configure(Opts) ->
  #config{heartbeat = proplists:get_value(heartbeat, Opts, 5000),
    heartbeat_timeout = proplists:get_value(heartbeat_timeout, Opts, 30000),
    session_timeout = proplists:get_value(session_timeout, Opts, 30000),
    protocol = proplists:get_value(protocol, Opts, socketio_data_protocol),
    opts = proplists:get_value(opts, Opts, undefined)
  }.

create(SessionId, SessionTimeout, Opts) ->
  recon_web_session_sup:start_child(SessionId, SessionTimeout, Opts).

find(SessionId) ->
  case ets:lookup(?SESSION_MANAGER_ETS, SessionId) of
    [] ->  {error, not_found};
    [{_, Pid}] -> {ok, Pid}
  end.

set_caller_and_pull_msg(Pid, Caller) ->
  gen_server:call(Pid, {set_caller_and_pull_msg, Caller}, infinity).

pull_msg_from_session(Pid) ->
  gen_server:call(Pid, {pull_msg_from_session}, infinity).

send_message(Pid, Sid, Message) when is_binary(Message) ->
  gen_server:cast(Pid, {send, Sid, {message, <<>>, <<>>, Message}}).

%%for test
send_message_to_client(Pid, Text) ->
  gen_server:cast(Pid, {message, Text}).

send_obj(Pid, Sid, Obj) ->
  gen_server:cast(Pid, {send, Sid, {json, <<>>, <<>>, Obj}}).

deliver_msg(Pid, Messages) when is_list(Messages) ->
  gen_server:call(Pid, {deliver_msg, Messages}, infinity).

refresh(Pid) ->
  gen_server:cast(Pid, {refresh}).

connect(Pid, Sid) ->
  gen_server:cast(Pid, {send, Sid, {connect, <<>>}}).

disconnect(Pid) ->
  gen_server:cast(Pid, {disconnect}).

unsub_caller(Pid, Caller) ->
  gen_server:call(Pid, {unsub_caller, Caller}).
%%--------------------------------------------------------------------
start_link(SessionId, SessionTimeout, Opts) ->
  gen_server:start_link(?MODULE, [SessionId, SessionTimeout, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([SessionId, SessionTimeout, Opts]) ->
  Self = self(),
  erlang:send(Self, register_in_ets),
  TRef = erlang:send_after(SessionTimeout, Self, session_timeout),
  {ok, #state{id = SessionId,
    registered = false,
    opts = Opts,
    session_timeout_tref = TRef,
    session_timeout = SessionTimeout}}.

%%--------------------------------------------------------------------
handle_call({set_caller_and_pull_msg, Caller}, _From,  State = #state{caller = undefined, messages = Messages}) ->
  State1 = refresh_session_timeout(State),
  {reply, lists:reverse(Messages), State1#state{caller = Caller, messages = []}};
handle_call({set_caller_and_pull_msg, _Caller}, _From,  State ) ->
  State1 = refresh_session_timeout(State),
  {reply, ?SESSION_IN_USE, State1};

handle_call({pull_msg_from_session}, _From,  State = #state{messages = Messages}) ->
  State1 = refresh_session_timeout(State),
  {reply, lists:reverse(Messages), State1#state{messages = []}};

handle_call({deliver_msg, Messages}, _From, State) ->
  State1 = refresh_session_timeout(State),
  process_messages(Messages, State1);

handle_call({unsub_caller, _Caller}, _From, State = #state{caller = undefined}) ->
  {reply, ok, State};
handle_call({unsub_caller, Caller}, _From, State = #state{caller = PrevCaller}) ->
  case Caller of
    PrevCaller -> {reply, ok, State#state{caller = undefined}};
    _ -> {reply, ok, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.
%%--------------------------------------------------------------------

handle_cast({refresh}, State) ->
  {noreply, refresh_session_timeout(State)};

handle_cast({send, Sid, Message}, State = #state{id = Sid, messages = Messages, caller = Caller}) ->
  case Caller of
    Pid when is_pid(Pid) -> recon_web_handle:notify_handler_have_new_message(Pid, self());
    undefined -> ok
  end,
  {noreply, State#state{messages = [Message|Messages]}};

handle_cast({message, Message}, State = #state{messages = Messages, caller = Caller}) ->
  case Caller of
    Pid when is_pid(Pid) -> recon_web_handle:notify_handler_have_new_message(Pid, self());
    undefined -> ok
  end,
  {noreply, State#state{messages = [Message|Messages]}};

handle_cast({disconnect}, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
handle_info(session_timeout, State) ->
  {stop, normal, State};

handle_info(register_in_ets, State =
  #state{id = SessionId, registered = false}) ->
  lager:info("recon_web_session:~p    register_in_ets Pid ~p ~n", [SessionId, self()]),
  case ets:insert_new(?SESSION_MANAGER_ETS, {SessionId, self()}) of
    true ->
      connect(self(), SessionId),
      {noreply, State#state{registered = true}};
    false -> {stop, session_id_exists, State}
  end;

handle_info(Info, State = #state{id = Id, registered = true}) ->
  lager:error("recon_web_session1 unknow message ~p~n", [Info, Id]),
  {noreply, State};

handle_info(_Info, State) ->
  lager:error("recon_web_session2 unknow message ~p~n", [_Info]),
  {noreply, State}.
%%--------------------------------------------------------------------
terminate(_Reason, State = #state{id = SessionId}) ->
  ets:delete(?SESSION_MANAGER_ETS, SessionId),
  {stop, normal, State}.
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
refresh_session_timeout(State = #state{session_timeout = Timeout, session_timeout_tref = TRef}) ->
  erlang:cancel_timer(TRef),
  NewTRef = erlang:send_after(Timeout, self(), session_timeout),
  State#state{session_timeout_tref = NewTRef}.

process_messages([], State) ->
  {reply, ok, State};
process_messages([Message|Rest], State = #state{id = Sid}) ->
  case Message of
    {disconnect, _EndPoint} ->
      {stop, normal, ok, State};
    {connect, _EndPoint} ->
      process_messages(Rest, State);
    disconnect ->
      {stop, normal, ok, State};
    heartbeat ->
      recon_server:request_recon_info(self(), Sid),
      process_messages(Rest, State);
%% todo handle message
    {message, <<>>, EndPoint, Obj} ->
      lager:info("from client message~p~n", [{EndPoint, Obj}]),
      process_messages(Rest, State);
    {json, <<>>, EndPoint, Obj} ->
      lager:info("from client json~p~n", [{EndPoint, Obj}]),
      process_messages(Rest, State);
    T ->
      lager:error("unknow:~p~n", [T]),
      process_messages(Rest, State)
  end.
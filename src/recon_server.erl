% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
% (c) Apache / CouchDB

% crypto server should be started

-module(recon_server).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([request_recon_info/2,
    request_system_info/2
        ]).

%%gen_server callback
-export([init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2]).

-define(SYSTEM_UPDATE_INTERVAL_SEC, 60 * 60 ).
-define(RECON_UPDATE_INTERVAL_SEC, 5).

-record(state, {system = [], system_update_time = 1000, recon = [], recon_update_time = 1000}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

request_recon_info(SessionPid, Sid) ->
    gen_server:cast(?MODULE, {request_recon_info, SessionPid, Sid}).

request_system_info(SessionPid, Sid) ->
    gen_server:cast(?MODULE, {request_system_info, SessionPid, Sid}).

%%callback

init([]) ->
    recon_alloc:set_unit(megabyte),
    {ok, #state{}, hibernate}.

handle_call(Request, _From, State) ->
    lager:error("~p:unknow call request~p~n", [?MODULE, Request]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({request_system_info, SessionPid, Sid}, State) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {[SystemInfo, _ReconInfo], NewState} = update(Now, State),
    recon_web_session:send_obj(SessionPid, Sid, SystemInfo),
    {noreply, NewState};

handle_cast({request_recon_info, SessionPid, Sid}, State) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {[_SystemInfo, ReconInfo], NewState} = update(Now, State),
    recon_web_session:send_obj(SessionPid, Sid, ReconInfo),
    {noreply, NewState};

handle_cast(Msg, State) ->
    lager:error("~p:unknow cast request~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("~p:unknow info request~p~n", [?MODULE, Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%INTERNAL FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update(Now, #state{system_update_time = SysUpdateTime, recon_update_time = ReconUpdateTime})
    when Now >= SysUpdateTime +?SYSTEM_UPDATE_INTERVAL_SEC andalso Now >= ReconUpdateTime + ?RECON_UPDATE_INTERVAL_SEC->
    NewSystemInfo = update_system_info(),
    NewReconInfo = update_recon_info(),
    NewState = #state{system = NewSystemInfo, recon = NewReconInfo, recon_update_time = Now, system_update_time = Now},
    {[NewSystemInfo, NewReconInfo], NewState};
update(Now, State = #state{recon_update_time = ReconUpdateTime, system =  SystemInfo})
    when Now >= ReconUpdateTime + ?RECON_UPDATE_INTERVAL_SEC->
    NewReconInfo = update_recon_info(),
    NewState = State#state{recon = NewReconInfo, recon_update_time = Now},
    {[SystemInfo, NewReconInfo], NewState};
update(Now, State = #state{system_update_time = SystemUpdateTime, recon = ReconInfo})
    when Now >= SystemUpdateTime + ?SYSTEM_UPDATE_INTERVAL_SEC->
    NewSystemInfo = update_system_info(),
    NewState = State#state{system = NewSystemInfo, system_update_time =  Now},
    {[NewSystemInfo, ReconInfo], NewState};
update(_Now, State = #state{system = SystemInfo, recon = ReconInfo}) ->
    {[SystemInfo, ReconInfo], State}.

update_system_info() ->
    SysInfo = recon_web_system:get_all_system_info(),
    recon_web_system:system_to_json(SysInfo).

update_recon_info() ->
    ReconInfo = recon_web_status:get_all_recon_info(),
    recon_web_status:recon_to_json(ReconInfo).

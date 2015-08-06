-module(recon_web_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 10, 10},
        [?CHILD(recon_web_uuids, worker),
          ?CHILD(recon_server, worker),
          ?CHILD(recon_web_session_sup, supervisor)
        ]}}.

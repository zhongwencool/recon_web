%%%-------------------------------------------------------------------
-module(recon_web_session_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

%% --------------------------------------------------------------------------

-spec start_link() -> ignore | {'ok', pid()} | {'error', any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
        [{undefined, {recon_web_session, start_link, []},
            temporary, 5000, worker, [recon_web_session]}]}}.

%%If the case of a simple_one_for_one supervisor,
%%the child specification defined in Module:init/1
%%will be used and ChildSpec should instead be an arbitrary list of terms List.
%%The child process will then be started by appending List to the existing start function
%%arguments, i.e. by calling apply(M, F, A ++ List) where {M, F, A}
%%is the start function defined in the child specification.
start_child(SessionId, SessionTimeout, Opts) ->
    supervisor:start_child(?MODULE, [SessionId, SessionTimeout, Opts]).

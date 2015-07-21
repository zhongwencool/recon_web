-module(recon_web_lib).

-export([pid_to_binary/1,urlencode_pid/1,
         port_to_binary/1,urlencode_port/1]).
-export([to_atom_or_binary/1,pid_or_port_to_binary/1]).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

urlencode_pid(Pid) ->
    http_uri:encode(pid_to_list(Pid)).

to_atom_or_binary(Value)when is_list(Value) ->
    list_to_binary(Value);
to_atom_or_binary(Value) ->
    Value.

urlencode_port(Port) ->
    http_uri:encode(erlang:port_to_list(Port)).

port_to_binary(Port) ->
    list_to_binary(erlang:port_to_list(Port)).


pid_or_port_to_binary(Controller) when is_pid(Controller) ->
    erlang:list_to_binary(erlang:pid_to_list(Controller));
pid_or_port_to_binary(Controller) ->
    erlang:list_to_binary(erlang:port_to_list(Controller)).


%%

%%========================================
%% Internal
%%========================================





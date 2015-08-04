
-module(recon_web_system).

-include("recon_web.hrl").
%% API
-export([get_all_system_info/0,
    get_system_info/1,
    system_to_json/1,
    system_to_prop/1
]).



get_all_system_info() ->
    get_system_info(?ALL_INFO_ITEMS).

get_system_info(Items) ->
    lists:foldl(fun(InfoItem, Acc) ->
        try erlang:system_info(InfoItem) of
            Value ->
                [{InfoItem, Value} | Acc]
        catch
            _:_ ->
                Acc
        end
    end, [], Items).

system_to_json(Systems) ->
    NewSystems = lists:foldl(fun(Sys, Acc) -> system_to_prop(Sys) ++ Acc end, [], Systems),
    NewSystems.

system_to_prop({PrimKey, Items})when PrimKey =:= check_io; PrimKey =:= garbage_collection ->
    PrimKeyStr = to_binary(PrimKey),
    [{<<PrimKeyStr/binary, "_", (to_binary(Key))/binary>>, Value} ||{Key, Value} <- Items];
system_to_prop({c_compiler_used, {Name, {Major, Minor, Patch}}}) ->
    [{<<"c_compiler_user", (list_to_binary(atom_to_list(Name)))/binary>>,
        list_to_binary("Major:" ++ integer_to_list(Major) ++ " Minor:" ++
            integer_to_list(Minor) ++ " Patch:" ++ integer_to_list(Patch))}];
system_to_prop({fullsweep_after, {_, After}}) ->
    [{fullsweep_after, After}];
system_to_prop({min_heap_size, {_, Size}}) ->
    [{min_heap_size, Size}];
system_to_prop({min_bin_vheap_size, {_, Size}}) ->
    [{min_bin_vheap_size, Size}];
system_to_prop({multi_scheduling_blockers, Blockers}) ->
    [{multi_scheduling_blockers, [pid_to_binary(Blocker) || Blocker <- Blockers]}];
system_to_prop({scheduler_bindings, Bindings}) ->
    [{scheduler_bindings, tuple_to_list(Bindings)}];
system_to_prop({Key, Value}) ->
    case io_lib:printable_latin1_list(Value) of
        true -> [{Key, list_to_binary(Value)}];
        false-> [{Key, Value}]
    end.
to_binary(Value) when is_atom(Value) ->
    list_to_binary(atom_to_list(Value));
to_binary(Value)when is_integer(Value) ->
    integer_to_binary(Value).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

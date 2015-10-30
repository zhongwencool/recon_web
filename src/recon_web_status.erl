
-module(recon_web_status).

-include("recon_web.hrl").
%% API
-export([get_all_recon_info/0,
  get_recon_info/1,
  recon_to_json/1
]).
-define(ALL_RECON_INFO, [node_stats_list, proc_count, port, inet_count, session_count, alloc_memory, cache_hit_rates]).
-define(COUNT, 10).
-define(INET_ATTR_LIST, ['sent_oct', 'recv_oct', 'sent_cnt', 'recv_cnt']).
-define(ALLOC_MEMORY_LIST, [used, allocated, unused, allocated_types, allocated_instances]).

get_all_recon_info() ->
  [begin get_recon_info(Item) end || Item<- ?ALL_RECON_INFO].


%%[{[{process_count, 1954},
%% {run_queue, 0},
%% {error_logger_queue_len, 0},
%% {memory_total, 922964152},
%% {memory_procs, 35629640},
%% {memory_atoms, 1383137},
%% {memory_bin, 203998912},
%% {memory_ets, 638408832}],
%% [{bytes_in, 0},
%% {bytes_out, 0},
%% {gc_count, 3},
%% {gc_words_reclaimed, 2560},
%% {reductions, 5292355},
%% {scheduler_usage, [{1, 0.8207977450105526},
%%                   {2, 0.08309223796554367},
%%                   {3, 1.0},
%%                   {4, 0.05474425434794147}]}]}]
get_recon_info(node_stats_list) ->
  [{ProcessInfos, MemInfos}] = recon:node_stats_list(1, 0),
  [{process_summary, ProcessInfos}, {mem_summary, MemInfos}];

get_recon_info(proc_count) ->
  MemoryList = recon:proc_count(memory, ?COUNT),
  BinMemoryList = recon:proc_count(binary_memory, ?COUNT),
  ReductionList = recon:proc_count(reductions, ?COUNT),
  HeapSizeList = recon:proc_count(total_heap_size, ?COUNT),
  NewMemoryList = recon_web_lib:pid_to_display(MemoryList),
  NewBinMemoryList = recon_web_lib:pid_to_display(BinMemoryList),
  NewReductionList = recon_web_lib:pid_to_display(ReductionList),
  NewHeapSizeList = recon_web_lib:pid_to_display(HeapSizeList),
  [{proc_count, [{memory, NewMemoryList}, {bin_memory, NewBinMemoryList},
    {reductions, NewReductionList}, {total_heap_size, NewHeapSizeList} ]}];

get_recon_info(port) ->
  PortInfo = recon:port_types(),
  NewPortInfo = [begin {list_to_binary(Key), Value} end|| {Key, Value} <- PortInfo],
  [{port_summary, NewPortInfo}];
get_recon_info(inet_count) ->
  [{inet_count, [begin
                   AttrSizes = recon:inet_count(Attr, ?COUNT),
                   {Attr, [begin {recon_web_lib:port_to_display(Port), Size} end||{Port, Size, _} <- AttrSizes]}
                 end ||Attr <-?INET_ATTR_LIST]}];

get_recon_info(session_count) ->
  [{session_count, ets:info(?SESSION_MANAGER_ETS, size)}];

get_recon_info(alloc_memory) ->
  [begin {Attr, recon_alloc:memory(Attr)} end || Attr<- ?ALLOC_MEMORY_LIST];

get_recon_info(cache_hit_rates) ->
  [{cache_hit_rates, [begin {list_to_binary("instance" ++ integer_to_list(Num)), [Hits, Calls]}
                      end|| {{instance, Num}, [_, {hits, Hits}, {calls, Calls}]} <- recon_alloc:cache_hit_rates()]}].

recon_to_json(Recons) ->
  Recons.

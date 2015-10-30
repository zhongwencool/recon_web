-module(recon_web_top).

-export([system_status/0, recon_status/0]).

system_status() ->
  Version = erlang:system_info(system_version),
  ProcLimit = to_list(erlang:system_info(process_limit)),
  ProcCount = to_list(erlang:system_info(process_count)),
  PortLimit = to_list(erlang:system_info(port_limit)),
  PortCount = to_list(erlang:system_info(port_count)),
  EtsLimit = to_list(erlang:system_info(ets_limit)),
  LogicalProc = to_list(erlang:system_info(logical_processors)),
  UseMem = to_m(recon_alloc:memory(used)),
  AlloctedMem = to_m(recon_alloc:memory(allocated)),
  UnunsedMem = to_m(recon_alloc:memory(unused)),
  io:format("~s Uptime:~s~n", [Version --"\n", uptime()]),
  io:format("~133.133.=s~n", [""]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["System Limit", "Limit", "System Count", "Count", "Memory Info", "Megabyte"]),
  io:format("~133.133.-s~n", [""]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Process Limit", ProcLimit, "Process Count", ProcCount, "Use Mem", UseMem]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Port Limit", PortLimit, "Port Count", PortCount, "Allocted Mem", AlloctedMem]),
  io:format("~-15.15s | ~-15.15s | ~-18.18s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Ets Limit", EtsLimit, "Logical Processors", LogicalProc, "Unuse Mem", UnunsedMem]),
  io:format("~133.133.=s~n", [""]).
%%

recon_status() ->
  [{ProcessSum, MemSum}] = recon:node_stats_list(1, 0),
  draw_memory_process_info(ProcessSum, MemSum),
  io:format("~133.133.=s~n", [""]),
  draw_cpu(MemSum),
  io:format("~133.133.=s~n", [""]),
  draw_process_rank(),
  ok.

%%========================================
%% Internal
%%========================================
-define(COUNT, 14).
draw_process_rank() ->
  MemoryList = recon:proc_count(memory, ?COUNT),
  BinMemoryList = recon:proc_count(binary_memory, ?COUNT),
  ReductionList = recon:proc_count(reductions, ?COUNT),
  HeapSizeList = recon:proc_count(total_heap_size, ?COUNT),
  Mem = recon_web_lib:pid_to_display(MemoryList),
  BinMem = recon_web_lib:pid_to_display(BinMemoryList),
  Reductions = recon_web_lib:pid_to_display(ReductionList),
  TotalHeapSize = recon_web_lib:pid_to_display(HeapSizeList),
  io:format("| ~-19.19s|~10.10s| ~-19.19s|~10.10s| ~-19.19s|~10.10s| ~-19.19s|~10.10s~n",
    ["Pid", "Memory", "Pid", "Reductions", "Pid", "Bin Memory", "Pid", "Total Heap Size"]),
  [begin
     {MemPid, MemVal} = lists:nth(Pos, Mem),
     {ReductionPid, ReductionsVal} = lists:nth(Pos, Reductions),
     {BinPid, BinVal} = lists:nth(Pos, BinMem),
     {HeapPid, HeapVal} = lists:nth(Pos, TotalHeapSize),
     io:format("| ~-19.19s|~10.10s| ~-19.19s|~10.10s| ~-19.19s|~-10.10s| ~-19.19s|~10.10s~n",
       [MemPid, to_list(MemVal), ReductionPid, to_list(ReductionsVal),
         BinPid, to_list(BinVal), HeapPid, to_list(HeapVal)])
   end|| Pos <- lists:seq(1, ?COUNT)].

draw_memory_process_info(ProcessSum, MemSum) ->
  TotalMem = to_m(proplists:get_value(memory_total, ProcessSum)),
  ProcMem = to_m(proplists:get_value(memory_procs, ProcessSum)),
  AtomMem = to_m(proplists:get_value(memory_atoms, ProcessSum)),
  BinMem = to_m(proplists:get_value(memory_bin, ProcessSum)),
  EtsMem = to_m(proplists:get_value(memory_ets, ProcessSum)),
  ProcessCount = integer_to_list(proplists:get_value(process_count, ProcessSum)),
  Runqueue = integer_to_list(proplists:get_value(run_queue, ProcessSum)),
  ErrorLogCount = integer_to_list(proplists:get_value(error_logger_queue_len, ProcessSum)),
  BytesIn = to_m(proplists:get_value(bytes_in, MemSum)),
  BytesOut = to_m(proplists:get_value(bytes_out, MemSum)),
  GcCount = to_m(proplists:get_value(gc_count, MemSum)),
  GcWordsReclaimed = to_m(proplists:get_value(gc_words_reclaimed, MemSum)),
  Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),

  io:format("~-15.15s | ~-20.20s | ~-15.15s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Memory", "Megabyte", "Process State", "Count", "Memory", "Megabyte"]),
  io:format("~133.133.-s~n", [""]),
  io:format("~-15.15s | ~-20.20s | ~-15.15s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Total", TotalMem, "Reductions", Reductions, "IO Output", BytesOut]),
  io:format("~-15.15s | ~-20.20s | ~-15.15s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Process", ProcMem, "Process Count", ProcessCount, "IO Input", BytesIn]),
  io:format("~-15.15s | ~-20.20s | ~-15.15s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Atom", AtomMem, "Run Queue", Runqueue, "Gc Count", GcCount]),
  io:format("~-15.15s | ~-20.20s | ~-15.15s | ~-20.20s | ~-20.20s | ~-20.20s~n",
    ["Ets", EtsMem, "Error Log Queue", ErrorLogCount, "Gc Words Reclaimed", GcWordsReclaimed]),
  io:format("~-15.15s | ~-20.20s~n",
    ["Binary", BinMem]).

draw_cpu(MemSum) ->
  CPU = proplists:get_value(scheduler_usage, MemSum),
  CPUNum = erlang:length(CPU),
  Inc = CPUNum div 2,
  [begin
     Percent1 = proplists:get_value(Seq, CPU),
     Percent2 = proplists:get_value(Seq + Inc, CPU),
     CPU1 = float_to_list(trunc(Percent1*100*1000)/1000, [{decimals, 2}]) ++ "%",
     CPU2 = float_to_list(trunc(Percent2*100*1000)/1000, [{decimals, 2}]) ++ "%",
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + Inc])),
     Process1 = lists:duplicate(trunc(Percent1 * 52), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 52), "|"),
     io:format("|~-3.3s[ ~-52.52s~s] |~-3.3s[ ~-52.52s~s]~n", [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
   end|| Seq <- lists:seq(1, Inc)].

to_m(M) ->
  float_to_list(trunc(M/(1024*1024)*1000)/1000, [{decimals, 4}]) ++ "M".
to_list(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
to_list(Val) -> Val.

uptime() ->
  uptime(get_uptime()).

uptime({D, {H, M, S}}) ->
  lists:flatten(io_lib:format("~p Days ~p:~p:~p", [D, H, M, S])).

get_uptime() ->
  {UpTime, _} = erlang:statistics(wall_clock),
  calendar:seconds_to_daystime(UpTime div 1000).


-define(ALL_INFO_ITEMS, [build_type , c_compiler_used , check_io , compat_rel , creation , debug_compiled , dist , dist_buf_busy_limit , driver_version , dynamic_trace , dynamic_trace_probes , elib_malloc , ets_limit , fullsweep_after , garbage_collection , heap_sizes , heap_type ,  kernel_poll ,  logical_processors, machine , min_heap_size , min_bin_vheap_size , modified_timing_level , multi_scheduling , multi_scheduling_blockers , otp_release , port_count , port_limit , process_count , process_limit ,  scheduler_bind_type , scheduler_bindings , scheduler_id , schedulers , smp_support , system_version , system_architecture , threads , thread_pool_size , trace_control_word , update_cpu_info , version , wordsize]).

%%session manager table ets
-define(SESSION_MANAGER_ETS, session_manager_ets).

-record(config, {heartbeat,
  heartbeat_timeout,
  session_timeout,
  callback,
  protocol,
  opts
}).

%%action mode
-define(CREATE_SESSION,create_session).
-define(SESSION_IN_USE,session_in_use).
-define(HEARTBEAT,heartbeat).
-define(DATA,data).
-define(WRONG_ACTION,error).
-define(SESSION_NOT_FIND,not_find).
-define(SEND,send).
-define(OK,ok).
-define(DISCONNECT,disconnect).


-define(TEXT_HEAD,
  [{<<"content-Type">>, <<"text/plain; charset=utf-8">>},
    {<<"Cache-Control">>, <<"no-cache">>},
    {<<"Expires">>, <<"Sat, 25 Dec 1999 00:00:00 GMT">>},
    {<<"Pragma">>, <<"no-cache">>},
    {<<"Access-Control-Allow-Credentials">>, <<"true">>},
    {<<"Access-Control-Allow-Origin">>, <<"null">>}]).
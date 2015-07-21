-module(recon_web_hal).
 
-include("recon_web.hrl").

%% API
-export([to_hal/2]).

-define(MEMBER_PORT(Href),Href =:= <<"/recon/inet_count/recv_cnt">>;
    Href =:= <<"recon/inet_count/recv_cnt">>;
    Href =:= <<"recon/inet_count/recv_oct">>;
    Href =:= <<"recon/inet_count/send_cnt">>;
    Href =:= <<"recon/inet_count/send_oct">>).
-define(MEMBER_PORT_RANK(Href),Href =:= <<"/inet_count/recv_cnt/rank">>;
    Href =:= <<"recon/inet_count/recv_cnt/rank">>;
    Href =:= <<"recon/inet_count/recv_oct/rank">>;
    Href =:= <<"recon/inet_count/send_cnt/rank">>;
    Href =:= <<"recon/inet_count/send_oct/rank">>).


to_hal(Ports,<<"/recon/port_types">> = Href) ->
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }}],
            embedded = [],
            properties = [{object, Ports}]
        },
    hal_serializer:to_json(Resource);

%%['recv_cnt','recv_oct','send_cnt','send_oct'],
to_hal(InetCounts,Href)when ?MEMBER_PORT(Href) ->
    {Links,Length} =
        lists:foldr(fun({InetPort,Num,_List},{Acc,Rank}) ->
            {[#hal_resource_data{
                rels = [#hal_rel_data{
                    name = self,
                    links = #hal_link_data{
                        href = << Href/binary,<<"/rank/">>/binary,
                        (integer_to_binary(Rank))/binary>>,
                        options = []
                    }}],
                embedded = [],
                properties = [recon_web_inet:port_to_obj(InetPort,Num)]
            }|Acc],Rank+1}
        end,{[],1},InetCounts),
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }}],
            embedded = [{inetcount,Links}],
            properties = [{object,[{total, Length}]}]
        },
    hal_serializer:to_json(Resource);

%%single port specific info
to_hal({InetPort,_Num,_List},Href)when ?MEMBER_PORT_RANK(Href) ->
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }}
            ],
            embedded = [],
            properties = [recon_web_inet:port_to_obj(InetPort)]
        },
    hal_serializer:to_json(Resource);

%% all processess info
to_hal(Processes,<<"/recon/processes">> = Href) ->
    Links = [begin
                 #hal_resource_data{
                     rels = [#hal_rel_data{
                         name = self,
                         links = #hal_link_data{
                             href = <<"/recon/process/",(list_to_binary(recon_web_lib:urlencode_pid(Process)))/binary>>,
                             options = []
                         }}],
                     embedded = [],
                     properties = [{object,[{pid,recon_web_lib:pid_to_binary(Process)}]}]
                 }
             end||Process <- Processes],
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }}],
            embedded = [{processes,Links}],
            properties = [{object,[{total, erlang:length(Processes)}]}]
        },
    hal_serializer:to_json(Resource);

%% single process info
to_hal(PidBin,<<"/recon/process">> = Href) ->
    Pid = list_to_pid(binary_to_list(PidBin)),
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = <<Href/binary,PidBin/binary>>,
                    options = []
                }}],
            embedded = [],
            properties = [recon_web_process:process_to_obj(Pid)]
        },
    hal_serializer:to_json(Resource);

%%Module
to_hal(Modules,<<"/recon/modules">> = Href) ->
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }},
                #hal_rel_data{
                    name = curies,
                    links = #hal_link_data{
                        href = <<"/relations/{rel}.html">>,
                        options = [{templated, true}, {name, recon_web}]
                    }}],
            embedded = [],
            properties = [{object, [begin {Key,recon_web_lib:to_atom_or_binary(Val)}end||{Key,Val}<-Modules]}]
        },
    hal_serializer:to_json(Resource);

%%System
to_hal(Systems,<<"/recon/system">> = Href) ->
    Resource =
        #hal_resource_data{
            rels = [#hal_rel_data{
                name = self,
                links = #hal_link_data{
                    href = Href,
                    options = []
                }}
            ],
            embedded = [],
            properties = recon_web_system:system_to_objs(Systems)
        },
    hal_serializer:to_json(Resource).

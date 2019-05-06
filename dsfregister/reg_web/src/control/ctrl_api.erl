-module(ctrl_api).
-export([handle_request/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("reg_server.hrl").
-include("define.hrl").

handle_request('GET',[""],_Args) ->
    usage();
handle_request('GET',["reginfo"],_Args) ->
    usage();
handle_request('GET',["reginfo",RegName],_Args) ->
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} -> {html,lists:flatten(io_lib:format("api query failed : convert name to exists atom failed, ~p",[RegName]))};
        DService ->
            case dbutil:read(svcdef,DService) of
                {atomic,[SvcDef]} ->
                    {atomic,Records} = dbutil:match({dservice_state,{svcreg,DService,'_','_'},'_','_','_'}),
                    SvcList = [{{svcaddr,H,P},S} || {dservice_state, {svcreg,_Name,H,P},_SubID,_Time,S} <- Records],
                    Json = subret_to_json(#subscribe_result{svcdef=SvcDef,svclist=SvcList}),
                    JsonStr = encode(Json),
                    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
                {atomic, []} -> {html,lists:flatten(io_lib:format("api query failed : service undefine, ~p",[RegName]))}
            end
    end;
handle_request('GET',["subscriber"],_Args) ->
    usage();
handle_request('GET',["subscriber",RegName],_Args) ->
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} -> {html,lists:flatten(io_lib:format("api query failed : convert name to exists atom failed, ~p",[RegName]))};
        DService ->
            case dbutil:do_query(qlc:q([Y|| X <- mnesia:table(subscribe_map),
                                            X#subscribe_map.regname =:= DService,
                                            Y <- mnesia:table(dsfclient),
                                            X#subscribe_map.subid =:= Y#dsfclient.subid])) of
                {atomic, Records} ->
                    Json = subscriber_to_json(Records, []),
                    JsonStr = encode(Json),
                    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
                Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
            end
    end;
handle_request('GET',["rely"],_Args) ->
    usage();
handle_request('GET',["rely", RegName],_Args) ->
    Json = query_rely(RegName),
    JsonStr = encode(Json),
    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
handle_request('GET',["stat"],_Args) ->
    Json = query_stat(undefined),
    JsonStr = encode(Json),
    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
handle_request('GET',["stat", RegName],_Args) ->
    Json = query_stat(RegName),
    JsonStr = encode(Json),
    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
handle_request('GET',["runtime"],_Args) ->
    Json = get_run_info(),
    JsonStr = encode(Json),
    [{status, 200}, {header, {"Content-Type", "application/json; charset=utf-8"}}, {html, JsonStr}];
handle_request('GET',_,_Args) ->
    usage().


query_rely(RegNameStr) ->
        RegName = erlang:list_to_existing_atom(RegNameStr),
        query_dservice_state_record(RegName).
    query_dservice_state_record(RegName) ->
        {atomic, Records} = dbutil:match(#dservice_state{svcreg={svcreg,RegName,'_','_'},
                                          subid='_',
                                          update_time='_',
                                          states='_'}),
        query_subscriber_record(RegName, Records).
    query_subscriber_record(RegName, DserviceRecords) ->
        {atomic, Records} = dbutil:do_query(qlc:q([Y|| X <- mnesia:table(subscribe_map),
                                        X#subscribe_map.regname =:= RegName,
                                        Y <- mnesia:table(dsfclient),
                                        X#subscribe_map.subid =:= Y#dsfclient.subid])),
         generate_json_from_record(RegName, DserviceRecords, Records).
    generate_json_from_record(RegName, DserviceRecords, SubscriberRecords) ->
        Rely = rely_service_record_to_tuple(RegName, DserviceRecords, []),
        BeRelyed = berelyed_service_record_to_tuple(RegName, SubscriberRecords, []),
        {obj, [{rely,Rely}, {berelyed,BeRelyed}]}.
        
rely_service_record_to_tuple(_RegName, [], List) ->
    sets:to_list(sets:from_list(List));
rely_service_record_to_tuple(RegName, [R|Rest], List) ->
    S = R#dservice_state.subid,
    case dbutil:do_query(qlc:q([X || X<- mnesia:table(subscribe_map),X#subscribe_map.subid =:= S])) of
        {atomic, Records} ->
            L = [list_to_binary(atom_to_list(Y#subscribe_map.regname)) || Y <- Records, Y#subscribe_map.regname =/= RegName],
            rely_service_record_to_tuple(RegName, Rest, L ++ List);
        _ ->
            rely_service_record_to_tuple(RegName, Rest, List)
    end.
berelyed_service_record_to_tuple(_RegName, [], List) ->
    sets:to_list(sets:from_list(List));
berelyed_service_record_to_tuple(RegName, [R|Rest], List) ->
    S = R#dsfclient.subid,
    case dbutil:do_query(qlc:q([X#dservice_state.svcreg || X <- mnesia:table(dservice_state), X#dservice_state.subid =:= S])) of
        {atomic, Records} ->
            L = [list_to_binary(atom_to_list(Y#svcreg.regname)) || Y <- Records, Y#svcreg.regname =/= RegName],
            berelyed_service_record_to_tuple(RegName, Rest, L ++ List);
        _ ->
            berelyed_service_record_to_tuple(RegName, Rest, List)
    end.


query_stat(undefined) ->
    {atomic, Records} = dbutil:do_query(qlc:q([X||X<-mnesia:table(svcdef)])),
    svcdef_records_to_tuple(Records,[]);
query_stat(RegNameStr) ->
    RegName = erlang:list_to_existing_atom(RegNameStr),
    T = build_tuple(RegName),
    {obj, [{stat, [T]}]}.
build_tuple(RegName) ->
    {ok, SCount} = webdao:get_subscribe_count(RegName),
    {ok, ICount} = webdao:get_svc_instance_count(RegName),
    {obj, [{regname,list_to_binary(atom_to_list(RegName))}, {inst_count, ICount}, {sub_count, SCount}]}.
svcdef_records_to_tuple([],List) ->
    {obj, [{stat, List}]};
svcdef_records_to_tuple([R|Rest],List) ->
    T = build_tuple(R#svcdef.regname),
    svcdef_records_to_tuple(Rest,[T|List]).

get_run_info() ->
        {atomic, Records} = dbutil:do_query(qlc:q([X||X<-mnesia:table(dsconfig),
                                                        is_list(X#dsconfig.name),
                                                        lists:prefix("stat", X#dsconfig.name)])),
        run_info_record_to_tuple(Records, []).
    run_info_record_to_tuple([], List) ->
        {obj, List};
    run_info_record_to_tuple([R|Rest], List) ->
        %T = ?JSON_FROM_RECORD(dsconfig, R),
%        #dsconfig{config=Props} = R,
%
%        T = {obj, [{regname,foldl(run_info_prop_to_tuple, [], Props)}]},

        #dsconfig{name=N, config=Props} = R,
        run_info_record_to_tuple(Rest, [{N, {obj, Props}} | List]).
%    run_info_prop_to_tuple(T, PropList) ->
%        {N, V} = T,
%        case N of
%            duration ->
%                {{_, H}, {_, S}} = T,
%                D = integer_to_list(H) ++ "." ++ integer_to_list(S),
%                {duration, list_to_binary(D)};
%            dsf_request ->
%                {N, V};
%            dsf_connect ->
%                {N, V}
%        end.

usage() ->
    Version = "version : 1.3",
    Url = "URL : http:IP:Port/interface.yaws/api/[type]/ServiceName",
    Type = "type : reginfo | subscriber | rely | stat | runtime",
    {html,lists:flatten(io_lib:format(" API usage <br> ~s <br><br> ~s <br> ~s <br>",[Version, Url, Type]))}.

encode(Json) ->
    JsonStr = jsonrfc:encode(Json),
    list_to_binary([JsonStr,"\n"]).

subret_to_json(#subscribe_result{svcdef=SvcDef, svclist=SvcList}) ->
    J1 = ?JSON_FROM_RECORD(svcdef, SvcDef),
    J2 = [{obj,[{addr,?JSON_FROM_RECORD(svcaddr, Addr)},{states,{obj,states_to_json(States)}}]}||{Addr,States}<-SvcList],
    ?JSON_FROM_RECORD(subscribe_result, #subscribe_result{svcdef=J1,svclist=J2}).
states_to_json(States) ->
    [{N,V}||{svcstat,N,V}<-States].

subscriber_to_json([], List) ->
    {obj, [{subscribers, List}]};
subscriber_to_json([R|Records], List) ->
    IP = inet_parse:ntoa(R#dsfclient.ip),
    IPBin = list_to_binary(IP),

    {S, T}=R#dsfclient.time,
    
    {{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(T),
    Time = lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,HH,MM,SS])),
    TimeBin = list_to_binary(Time),

    StateBin = list_to_binary(atom_to_list(S)),

    Name = R#dsfclient.name,
    Ver = R#dsfclient.ver,
    Port = R#dsfclient.port,

    J = {obj, [{name, Name},{ver, Ver},{ip, IPBin},{port, Port},{status,StateBin},{time, TimeBin}]},
    subscriber_to_json(Records, [J | List]).


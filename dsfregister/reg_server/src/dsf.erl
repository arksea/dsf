%%%-------------------------------------------------------------------
%%% $Date: 2015-08-13 09:48:39 +0800 (周四, 13 八月 2015) $
%%% $Author: xulinrong_100407 $
%%% Socket Server for 'Distributed Service Framework'
%%%-------------------------------------------------------------------
-module(dsf).

-behaviour(gen_server).
-vsn("$Revision: 70196 $").

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% proc_lib Callback
-export([init_it/1]).

-include("define.hrl").
-include("reg_server.hrl").
-include_lib("stdlib/include/qlc.hrl").
-record(state, {sock, subid, name, ver, peername,tick=false,ticktimeout=0}).

%-define(DEBUG_PACKET(Msg, Details), alarm_handler:set_alarm({log_info,Msg,Details,?MODULE,?LINE})).
-define(DEBUG_PACKET(Msg, Details), donothing).
-define(CHECK_HEARTBEAT_DELAY, 30000).


%%====================================================================
%% API
%%====================================================================
start_link(StartArgs) ->
    {ok, proc_lib:spawn_link(?MODULE, init_it, [StartArgs])}.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init_it(StartArgs) ->
    case catch init(StartArgs) of
        {ok, State} ->
            gen_server:enter_loop(?MODULE, [], State);
        {ok, State, Timeout} ->
            gen_server:enter_loop(?MODULE, [], State, Timeout);
        {stop, Reason} ->
            exit(Reason);
        ignore ->
            exit(normal);
        {'EXIT', Reason} ->
            exit(Reason);
        Else ->
            Error = {bad_return_value, Else},
            exit(Error)
    end.
init(_StartArgs) ->
    process_flag(trap_exit,true),
    receive
        {go, Sock} ->
            ok = inet:setopts(Sock, [{active, once},{nodelay, true}, {packet, line}, {packet_size, 64 * 1024},{recbuf, 64 * 1024}]),
            case wait_for_login(Sock) of
                {ok, SubID, Name, Ver} ->
                    ok = inet:setopts(Sock, [{active, once}]),
                    erlang:send_after(?CHECK_HEARTBEAT_DELAY,self(),check_heartbeat),
                    {ok, PeerName} = inet:peername(Sock),
                    {ok, #state{sock=Sock, subid=SubID, name=Name, ver=Ver, peername=PeerName}};
                {error, Reason} ->
                    PeerInfo = inet:peername(Sock),
                    ?LOG_WARNING("login_failed", [{{error, Reason}, PeerInfo}]),
                    gen_tcp:close(Sock),
                    {stop, normal};
                {stop, _Reason} ->
                    gen_tcp:close(Sock),
                    {stop, normal};
                Other ->
                    gen_tcp:close(Sock),
                    Other
            end
    after 10000 ->
        {stop, wait_go_timeout}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Line}, State=#state{sock=Sock,peername=Peer}) ->
    case binary:split(Line,<<"=">>) of
        [Name,Data] ->
            if
                Name =/= <<"tick">> ->
                    ?LOG_DEBUG("socket receive line",[Line, Peer]),
                    perf_stat:dsf_request(Name);
                true -> ok
            end,
            case decode(Data) of
                {ok, Msg} ->
                    NewState = handle_tcp_message(Name, Msg, State),
                    ok = inet:setopts(Sock, [{active, once}]),
                    {noreply, NewState};
                Error ->
                    ok = send(Sock,<<"protocol_error">>,Line),
                    {stop, {protocol_error,{Error,Line},Peer}, State}
            end;
        _ ->
            ok = send(Sock,<<"protocol_error">>,Line),
            {stop, {protocol_error, Line, Peer}, State}
    end;
handle_info({tcp_closed, Sock}, State=#state{sock=Sock}) ->
    {stop, normal, State};
handle_info({tcp_error, Sock, Reason}, State=#state{sock=Sock}) ->
    {stop, {tcp_error, Reason},State};
handle_info({notify_svc_state, SvcStates}, State=#state{sock=Socket}) ->
    Json = svcstates_to_json(SvcStates),
    ok = send(Socket,<<"notify_svc_state">>,Json),
    {noreply, State};
handle_info({notify_svcdef_update, Svcdef}, State=#state{sock=Socket}) ->
    Json = ?JSON_FROM_RECORD(svcdef, Svcdef),
    ok = send(Socket,<<"notify_svcdef_update">>,Json),
    {noreply, State};
handle_info(check_heartbeat, State=#state{tick=T,ticktimeout=TO,peername=Peer}) ->
    case T of
        true  ->
            erlang:send_after(?CHECK_HEARTBEAT_DELAY,self(),check_heartbeat),
            {noreply, State#state{tick=false,ticktimeout=0}};
        false ->
            if TO < 3 ->
                    erlang:send_after(?CHECK_HEARTBEAT_DELAY,self(),check_heartbeat),
                    {noreply, State#state{ticktimeout=TO+1}};
                true ->
                    {stop, {ticktimeout, Peer}, State}
            end
    end;
handle_info(Info, State=#state{peername=Peer}) ->
    {stop, {unknown_info, Info, Peer}, State}.

terminate(_Reason, #state{sock=Sock,subid=SubID, name=Name, ver=Ver, peername=PeerName}) ->
    if
        Sock =:= undifned ->
            ok;
        true ->
            gen_tcp:close(Sock),
            {Address, Port} = PeerName,
            {atomic,ok} = dbutil:write(dsfclient, {dsfclient, SubID, Name, Ver, Address, Port, {offline, now()}}),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
handle_tcp_message(<<"tick">>, ID, State=#state{sock=Sock}) ->
    send(Sock,<<"tack">>,ID),
    State#state{tick=true};
handle_tcp_message(<<"subscribe">>,RegName, State=#state{sock=Sock,peername=_Peer,subid=SubID}) ->
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} ->
            ok = send(Sock,<<"error_result">>,<<"subscribe a not defined service: ",RegName/binary>>),
            ?LOG_WARNING("subscribe a not defined service",[RegName,{peer,_Peer}]);
        DService ->
            case dbutil:read(svcdef,DService) of
                {atomic,[SvcDef]} ->
                    {atomic,ok} = dbutil:write(subscribe_map,#subscribe_map{subid=SubID, regname=DService}),
                    {atomic,Records} = dbutil:match({dservice_state,{svcreg,DService,'_','_'},'_','_','_'}),
                    SvcList = [{{svcaddr,H,P},S} || {dservice_state, {svcreg,_Name,H,P},_SubID,_Time,S} <- Records],
                    Json = subret_to_json(#subscribe_result{svcdef=SvcDef,svclist=SvcList}),
                    ok = send(Sock,<<"subscribe_result">>,Json);
                {atomic, []} ->
                    ok = send(Sock,<<"error_result">>,<<"subscribe a not defined service: ",RegName/binary>>),
                    ?LOG_WARNING("subscribe a not defined service",[RegName,{peer,_Peer}])
            end
    end,
    State;
handle_tcp_message(<<"add_svcdef">>, [DefJson,IDL], State) ->
    Svcdef = ?JSON_TO_RECORD(svcdef, DefJson),
    do_add_svcdef(Svcdef, IDL, State);
handle_tcp_message(<<"update_svcdef">>, Json, State=#state{sock=Sock,peername=_Peer}) ->
    Svcdef = #svcdef{regname=RegNameBin} = ?JSON_TO_RECORD(svcdef, Json),
    case regsvc_misc:regname_to_exists_atom(RegNameBin) of
        {error,badarg} ->
            ok = send(Sock, <<"error_result">>,<<"update a not defined service: ",RegNameBin/binary>>),
            ?LOG_WARNING("update a not defined service",[RegNameBin,{peer,_Peer}]),
            State;
        RegName ->
            do_update_svcdef(Svcdef#svcdef{regname=RegName}, State)
    end;
handle_tcp_message(<<"update_svcdef_idl">>, [DefJson,IDL], State=#state{sock=Sock,peername=_Peer}) ->
    Svcdef = #svcdef{regname=RegNameBin} = ?JSON_TO_RECORD(svcdef, DefJson),
    case regsvc_misc:regname_to_exists_atom(RegNameBin) of
        {error,badarg} ->
            ok = send(Sock, <<"error_result">>,<<"update a not defined service: ",RegNameBin/binary>>),
            ?LOG_WARNING("update a not defined service",[RegNameBin,{peer,_Peer}]),
            State;
        RegName ->
            do_update_svcdef_idl(Svcdef#svcdef{regname=RegName}, IDL, State)
    end;
handle_tcp_message(<<"del_svcdef">>, RegNameBin, State=#state{sock=Sock,peername=_Peer}) ->
    case regsvc_misc:regname_to_exists_atom(RegNameBin) of
        {error,badarg} ->
            ok = send(Sock, <<"succeed">>,<<"true">>),
            ?LOG_WARNING("del a not defined service",[RegNameBin,{peer,_Peer}]);
        RegName ->
            do_del_svcdef(RegName, State)
    end,
    State;
handle_tcp_message(<<"query_svcdef">>, RegName, State=#state{sock=Sock,peername=_Peer}) ->
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} ->
            ok = send(Sock,<<"error_result">>,<<"query a not defined service: ",RegName/binary>>),
            ?LOG_WARNING("query a not defined service",[RegName,{peer,_Peer}]);
        DService ->
            {atomic,[SvcDef]} = dbutil:read(svcdef, DService),
            Json = ?JSON_FROM_RECORD(svcdef,SvcDef),
            ok = send(Sock,<<"query_svcdef_result">>,Json)
    end,
    State;
handle_tcp_message(<<"query_svc_state">>, RegName, State=#state{sock=Sock,peername=_Peer}) ->
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} ->
            ok = send(Sock,<<"error_result">>,<<"query a not defined service: ",RegName/binary>>),
            ?LOG_WARNING("query a not defined service",[RegName,{peer,_Peer}]);
        DService ->
            do_query_svc_state(DService, State)
    end,
    State;
handle_tcp_message(<<"report_svc_state">>,Json, State=#state{sock=Sock,peername=_Peer}) ->
    #svc_report{svc=SvcJsonObj,name=N,value=V} = ?JSON_TO_RECORD(svc_report, Json),
    SvcRegBin = ?JSON_TO_RECORD(svcreg,SvcJsonObj),
    #svcreg{regname=RegName} = SvcRegBin,
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} ->
            ?LOG_WARNING("report a not defined service",[RegName,{peer,_Peer}]),
            ok = send(Sock,<<"error_result">>,<<"report a not defined service: ",RegName/binary>>);
        DService ->
            SvcReg = SvcRegBin#svcreg{regname=DService},
            dservice:report_state(SvcReg, [{svcstat,N,V}])
    end,
    State;
handle_tcp_message(<<"set_svc_state">>, Json, State=#state{sock=Sock,peername=_Peer}) ->
    #set_svc_state{regname=RegName, addr=A_J, states=States} = ?JSON_TO_RECORD(set_svc_state, Json),
    case regsvc_misc:regname_to_exists_atom(RegName) of
        {error,badarg} ->
            ?LOG_WARNING("set a not defined service state",[RegName,{peer,_Peer}]),
            ok = send(Sock,<<"error_result">>,<<"set a not defined service: ",RegName/binary>>);
        DService ->
            A = ?JSON_TO_RECORD(svcaddr,A_J),
            #svcaddr{host=H, port=P} = A,
            case dservice:set_svc_state(#svcreg{regname=DService, host=H, port=P}, [?JSON_TO_RECORD(svcstat,S) || S <- States]) of
                ok ->
                    ok = send(Sock, <<"succeed">>,<<"true">>);
                Reason ->
                    Err = lists:flatten(io_lib:format("~p", [Reason])),
                    ok = send(Sock, <<"error_result">>, list_to_binary(Err))
            end
    end,
    State;
handle_tcp_message(<<"regsvc">>, Json, State=#state{sock=Sock,peername=Peer}) ->
    SvcRegBin = ?JSON_TO_RECORD(svcreg, Json),
    #svcreg{regname=RegNameBin, host=IPBin, port=Port} = SvcRegBin,
    if Port < 80 orelse Port > 65535 ->
            ?LOG_ERROR("regsvc port error",[{regsvc, Json},{peer,Peer}]),
            ok = send(Sock,<<"error_result">>,<<"regsvc port error", RegNameBin/binary>>);
        true ->
            IP = binary_to_list(IPBin),
            ?LOG_INFO("service registered", [SvcRegBin]),
            case inet_parse:ipv4strict_address(IP) of
                {ok, _} ->
                    do_regsvc(SvcRegBin, State);
                {error, einval} ->
                    ?LOG_ERROR("regsvc ip unidentified",[{regsvc, Json},{peer,Peer}]),
                    ok = send(Sock,<<"error_result">>,<<"regsvc ip unidentified", RegNameBin/binary>>)
            end
    end,
    State;
handle_tcp_message(<<"unregsvc">>,Json, State=#state{sock=Sock,peername=Peer}) ->
    SvcRegBin = ?JSON_TO_RECORD(svcreg, Json),
    #svcreg{regname=RegNameBin} = SvcRegBin,
    case regsvc_misc:regname_to_exists_atom(RegNameBin) of
        {error,badarg} ->
            ?LOG_ERROR("unregister a not defined service",[RegNameBin,{peer,Peer}]),
            ok = send(Sock,<<"error_result">>,<<"unregister a not defined service",RegNameBin/binary>>);
        DService ->
            SvcReg = SvcRegBin#svcreg{regname=DService},
            ok = dservice:unregister_service(SvcReg),
            ok = send(Sock,<<"succeed">>,<<"true">>)
    end,
    State;
handle_tcp_message(Name,Msg, State=#state{sock=Sock,peername=Peer}) ->
    ?LOG_ERROR("unknow message",[Name,Msg,{peer,Peer}]),
    ok = send(Sock,<<"error_result">>,<<"unknow message::",Name/binary,"=",Msg/binary>>),
    State.



%%---------------------------------------------------------------------
-spec wait_for_login(Socket::port()) -> {ok, SubId::list()} | {error, Reason::term()}.
wait_for_login(Socket) ->
        receive
            {tcp, Socket, Line} ->
                {ok, Peer} = inet:peername(Socket),
                ?LOG_INFO("client login",[Line, Peer]),
                check_max_online_dsf(Socket, Line, Peer);
            {tcp_closed, _Socket} ->
                %%应对LVS的拨测，连接后立即断开的行为
                {stop, normal}
        after 5000 ->
            send(Socket,<<"login_failed">>,<<"wait_for_login_timeout">>),
            {error, wait_for_login_timeout}
        end.
    check_max_online_dsf(Socket, Line, Peer) ->
        MaxOnline = regsvc_misc:get_env(max_online_dsf, 5000),
        Counts = supervisor:count_children(dsf_sup),
        {workers,DsfCount} = lists:keyfind(workers, 1, Counts),
        if DsfCount >= MaxOnline ->
                {error, {client_over_maxcount, DsfCount}};
           true ->
                handle_login_info(Socket, Line, Peer)
        end.
    handle_login_info(Socket, Line, Peer) ->
        case binary:split(Line,<<"=">>) of
            [<<"login">>,Bin] ->
                perf_stat:dsf_request(<<"login">>),
                {ok, Content} = decode(Bin),
                check_version_protocol_is_new(is_tuple(Content), Socket, Content, Peer);
            _ ->
                send(Socket,<<"login_failed">>,<<"login_protocol_error">>),
                {error, {login_protocol_error, Line, Peer}}
        end.
    check_version_protocol_is_new(false, Socket, SubID, Peer) ->
        decide_renew_subscribe(Socket, SubID, undefined, undefined, Peer);
    check_version_protocol_is_new(true, Socket, {_, ProtocolList}, Peer) ->
        Version = lists:keyfind("version", 1, ProtocolList),
        parse_login_protocol(Version, Socket, ProtocolList, Peer).
    parse_login_protocol({_, ProtocolVer = <<"1.0">>}, Socket, ProtocolList, Peer) ->
        parse_name_and_subid(Socket, ProtocolVer, ProtocolList, Peer);
    parse_login_protocol({_, ProtocolVer = <<"1.1">>}, Socket, ProtocolList, Peer) ->
        parse_name_and_subid(Socket, ProtocolVer, ProtocolList, Peer);
    parse_login_protocol(_Other, Socket, ProtocolList, _Peer) ->
        send(Socket,<<"login_failed">>,<<"unsupport_protocol_ver">>),
        {error, {unsupport_protocol_ver, ProtocolList}}.
    parse_name_and_subid(Socket, ProtocolVer, ProtocolList, Peer) ->
        NameTuple = lists:keyfind("name", 1, ProtocolList),
        SubIDTuple = lists:keyfind("subid", 1, ProtocolList),
        if NameTuple =:= false orelse SubIDTuple =:= false ->
                send(Socket,<<"login_failed">>,<<"login_protocol_invalid">>),
                {error, {login_protocol_invalid, ProtocolList}};
            true ->
                {_, Name} = NameTuple,
                {_, SubID} = SubIDTuple,
                decide_renew_subscribe(Socket, SubID, Name, ProtocolVer, Peer)
        end.
    decide_renew_subscribe(Socket, SubID, Name, ProtocolVer, Peer) ->
        %%为了保证信息一致性，要求客户端不能持久化SubID，SubID的生命周期为客户端进程，
        %%与订阅信息一起保存，一起失效，当失效后必须重新发起所有订阅
        case dbutil:read(dsfclient, SubID) of
            {atomic,[Sub=#dsfclient{subid=SubID, time={online, _}}]} ->
                ?LOG_ERROR("the login client's state is online",[Sub, Peer]),
                renew_subscribe(Socket, SubID, Name, ProtocolVer, Peer);
            {atomic,[Sub=#dsfclient{subid=SubID, time={offline, Time}}]} ->
                Diff = regsvc_misc:now_diff_sec(Time),
                if Diff>180 ->
                       ?LOG_INFO("the client offline timeout, need renew subscribe",[Sub, Peer]),
                       renew_subscribe(Socket, SubID, Name, ProtocolVer, Peer);
                   true ->
                       %掉线后180秒内重连的不做同步处理
                       login_succeed(Socket,SubID, Name, ProtocolVer, Peer)
                end;
            %新登录
            _Other ->
                {atomic, NewSubID} = get_next_subid(),
                renew_subscribe(Socket, NewSubID, Name, ProtocolVer, Peer)
        end.
    renew_subscribe(Socket, SubID, Name, ProtocolVer, Peer) ->
        ok = send(Socket,<<"sync_sublist">>,true),
        wait_for_sublist(Socket, SubID, Name, ProtocolVer, Peer).
    wait_for_sublist(Socket, SubID,  Name, ProtocolVer, Peer) ->
        ok = inet:setopts(Socket, [{active, once}]),
        receive
            {tcp, Socket, Line} ->
                ?LOG_INFO("client renew subscribe",[Line, Peer]),
                handle_sublist(Socket, Line, SubID, Name, ProtocolVer, Peer);
            {tcp_closed, _Socket} ->
                {error, {tcp_closed, wait_for_sublist, Peer}}
        after 5000 ->
            send(Socket,<<"login_failed">>,<<"wait_for_sublist_timeout">>),
            {error, wait_for_sublist_timeout, Peer}
        end.
    handle_sublist(Socket, Line, SubID, Name, ProtocolVer, Peer) ->
        perf_stat:dsf_request(<<"update_sublist">>),
        case binary:split(Line,<<"=">>) of
            [<<"update_sublist">>,SubListBin] ->
                {ok,SubList} = decode(SubListBin),
                NewList = regname_list_to_atom(SubList,[]),
                [dbutil:write(subscribe_map, {subscribe_map,SubID, RegName})||RegName<-NewList],
                login_succeed(Socket,SubID, Name, ProtocolVer, Peer);
            _ ->
                send(Socket,<<"login_failed">>,<<"renewsubscribe_protocol_error">>),
                {error, {renew_subscribe_protocol_error, Line, Peer}}
        end.
    login_succeed(Socket,SubID, Name, ProtocolVer, Peer={Address, Port}) ->
        {atomic,ok} = dbutil:write(dsfclient, #dsfclient{subid=SubID,
                                                         name=Name,
                                                         ver=ProtocolVer,
                                                         ip=Address,
                                                         port=Port,
                                                         time={online, now()}}),
        ?LOG_INFO("client login succeed", [SubID, Name, Peer]),
        yes = global:register_name({dsfclient,SubID}, self()),
        ok = send(Socket,<<"login_succeed">>,SubID),
        {ok, SubID, Name, ProtocolVer}.

%%--------------------------------------------------------------------
do_add_svcdef(Svcdef=#svcdef{regname=RegNameBin}, IDL, State=#state{sock=Sock,peername=Peer}) ->
    RegName = erlang:binary_to_atom(RegNameBin,latin1),
    case ds_manager:add_svcdef(Svcdef#svcdef{regname=RegName}, IDL) of
        ok ->
            ?LOG_INFO("add service define succeed", [RegName,{peer,Peer}]),
            ok = send(Sock, <<"succeed">>,<<"true">>);
        {error, Reason} ->
            ?LOG_ERROR("add service define failed", [RegName, {peer,Peer}, {error, Reason}]),
            Err = lists:flatten(io_lib:format("~p", [Reason])),
            ok = send(Sock, <<"error_result">>, list_to_binary(Err))
    end,
    State.

do_del_svcdef(RegName, State=#state{sock=Sock,peername=Peer}) ->
    case ds_manager:del_svcdef(RegName) of
        ok ->
            ?LOG_INFO("delete service define succeed", [RegName,{peer,Peer}]),
            ok = send(Sock, <<"succeed">>,<<"true">>);
        {error, Reason} ->
            ?LOG_ERROR("delete service define failed", [RegName, {peer,Peer}, {error, Reason}]),
            Err = lists:flatten(io_lib:format("~p", [Reason])),
            ok = send(Sock, <<"error_result">>, list_to_binary(Err))
    end,
    State.

do_update_svcdef_idl(Svcdef=#svcdef{regname=RegName}, IDL, State=#state{sock=Sock,peername=Peer}) ->
    case ds_manager:update_svcdef_idl(Svcdef, IDL) of
        ok ->
            ?LOG_INFO("update service define succeed", [RegName,{peer,Peer}]),
            ok = send(Sock, <<"succeed">>,<<"true">>);
        {error, Reason} ->
            ?LOG_WARNING("WARNING: update service define and idl failed",[RegName,{peer,Peer}, {error, Reason}]),
            Err = lists:flatten(io_lib:format("~p", [Reason])),
            ok = send(Sock, <<"error_result">>, list_to_binary(Err))
    end,
    State.

do_update_svcdef(Svcdef=#svcdef{regname=RegName}, State=#state{sock=Sock,peername=Peer}) ->
    case ds_manager:update_svcdef(Svcdef) of
        ok ->
            ?LOG_INFO("update service define succeed", [RegName,{peer,Peer}]),
            ok = send(Sock, <<"succeed">>,<<"true">>);
        {error, Reason} ->
            ?LOG_WARNING("WARNING: update service define failed",[RegName,{peer,Peer}, {error, Reason}]),
            Err = lists:flatten(io_lib:format("~p", [Reason])),
            ok = send(Sock, <<"error_result">>, list_to_binary(Err))
    end,
    State.

%%--------------------------------------------------------------------
do_query_svc_state(RegName, #state{sock=Sock,peername=Peer}) ->
    case dbutil:read(svcdef,RegName) of
        {atomic,[SvcDef]} ->
            {atomic,Records} = dbutil:match({dservice_state,{svcreg,RegName,'_','_'},'_','_','_'}),
            SvcList = [{{svcaddr,H,P},S} || {dservice_state, {svcreg,_Name,H,P},_SubID,_Time,S} <- Records],
            Json = subret_to_json(#subscribe_result{svcdef=SvcDef,svclist=SvcList}),
            ok = send(Sock,<<"query_svc_state_result">>,Json);
        Error ->
            ?LOG_ERROR("query service state failed",[Error, RegName,{peer,Peer}]),
            ok = send(Sock,<<"error_result">>,<<"query a not exists service">>)
    end.

%%--------------------------------------------------------------------
do_regsvc(SvcRegBin=#svcreg{regname=RegNameBin}, #state{subid=SubID, sock=Sock,peername=Peer}) ->
    case regsvc_misc:regname_to_exists_atom(RegNameBin) of
        {error,Reason} ->
            ?LOG_ERROR("register a not defined service",[RegNameBin,{error,Reason},{peer,Peer}]),
            ok = send(Sock,<<"error_result">>,<<"register a not defined service",RegNameBin/binary>>);
        DService ->
            SvcReg = SvcRegBin#svcreg{regname=DService},
            ok = dservice:register_service(SvcReg, SubID),
            ok = send(Sock,<<"succeed">>,<<"true">>)
    end.

%%--------------------------------------------------------------------
-spec decode(binary()) -> {ok|protocol_error,json()} | json().
decode(<<>>) ->
    {protocol_error,<<>>};
decode(JsonStr) ->
    {ok, Json, []} = jsonrfc:decode(JsonStr),
    {ok, Json}.

encode(Json) ->
    JsonStr = jsonrfc:encode(Json),
    list_to_binary([JsonStr,"\n"]).


svcstates_to_json({#svcreg{regname=RegName,host=Host,port=Port},NameValueList}) ->
    {obj,[{regname, RegName},{addr, {obj, [{host,Host},{port,Port}]}},{states,[one_svcstate_to_json(S)||S<-NameValueList]}]}.
one_svcstate_to_json({svcstat,Name,Value}) ->
    {obj,[{name,Name},{value,Value}]}.

subret_to_json(#subscribe_result{svcdef=SvcDef, svclist=SvcList}) ->
    J1 = ?JSON_FROM_RECORD(svcdef, SvcDef),
    J2 = [{obj,[{addr,?JSON_FROM_RECORD(svcaddr, Addr)},{states,{obj,states_to_json(States)}}]}||{Addr,States}<-SvcList],
    ?JSON_FROM_RECORD(subscribe_result, #subscribe_result{svcdef=J1,svclist=J2}).
states_to_json(States) ->
    [{N,V}||{svcstat,N,V}<-States].

send(Sock,<<"tack">>,JsonMsg) ->
    JsonStr = encode(JsonMsg),
    Pack = <<"tack","=",JsonStr/binary>>,
    gen_tcp:send(Sock,Pack);
send(Sock,Name,JsonMsg) ->
    JsonStr = encode(JsonMsg),
    Pack = <<Name/binary,"=",JsonStr/binary>>,
    {ok,Peer} = inet:peername(Sock),
    ?LOG_DEBUG("socket send line",[Pack, Peer]),
    gen_tcp:send(Sock,Pack).

regname_list_to_atom([],List) ->
    List;
regname_list_to_atom([RegName|Rest], List) ->
    try erlang:binary_to_existing_atom(RegName,latin1) of
        Name -> regname_list_to_atom(Rest, [Name|List])
    catch
        _:_ -> regname_list_to_atom(Rest, List)
    end.

get_next_subid() ->
    Fun = fun() -> do_get_next_subid() end,
    dbutil:mnesia_transaction(Fun).
do_get_next_subid() ->
    case qlc:e(qlc:q([X||X<-mnesia:table(dsconfig),X#dsconfig.name =:= subid])) of
        [{_,_,ID}] ->
            ok = mnesia:write(dsconfig, {dsconfig, subid, ID+1}, write),
            list_to_binary(integer_to_list(ID));
        [] ->
            ok = mnesia:write(dsconfig, {dsconfig, subid, 2}, write),
            <<"1">>;
        {error, M, R} -> {error, M, R};
        Other -> {error, Other}
    end.

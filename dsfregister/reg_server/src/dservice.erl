%%%-------------------------------------------------------------------
%%% $Date: 2015-08-13 09:48:39 +0800 (周四, 13 八月 2015) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(dservice).
-vsn("$Revision: 70196 $").
-behaviour(gen_server).



-include("define.hrl").
-include("reg_server.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(CHECK_ONLINE_DELAY, 30000).
-define(CHECK_OFFLINE_DELAY, 600000).

-record(state, {def}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2, 
        report_state/2, set_svc_state/2, notify_svcdef_update/1,
        register_service/2, unregister_service/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(ServerName,StartArgs) ->
    gen_server:start_link({local, ServerName}, ?MODULE, StartArgs, []).

register_service(SvcReg=#svcreg{regname=Service}, SubID) ->
    gen_server:call(Service,{register_service,SvcReg, SubID}).

unregister_service(SvcReg=#svcreg{regname=Service}) ->
    gen_server:call(Service,{unregister_service,SvcReg}).

report_state(SvcReg=#svcreg{regname=Service}, NameValueList) ->
    gen_server:cast(Service, {report_state, SvcReg, NameValueList}).

set_svc_state(SvcReg=#svcreg{regname=Service}, NameValueList) ->
    gen_server:call(Service, {set_svc_state, SvcReg, NameValueList}).

notify_svcdef_update(Svcdef=#svcdef{regname=Service}) ->
    gen_server:cast(Service, {notify_svcdef_update, Svcdef}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init({Def,_Args}) ->
    erlang:send_after(?CHECK_ONLINE_DELAY * 6,self(),check_online),
    erlang:send_after(?CHECK_ONLINE_DELAY,self(),check_offline),
    {ok,#state{def=Def}}.


handle_call({register_service, SvcReg, SubID}, _From, State) ->
    do_register_service(SvcReg, SubID),
    {reply, ok, State};
handle_call({unregister_service, SvcReg}, _From, State) ->
    do_unregister_service(SvcReg),
    {reply, ok, State};
handle_call({set_svc_state, SvcReg, NameValueList}, _From, State) ->
    case replace_state(SvcReg, NameValueList, true) of
        true ->
            {reply, ok, State};
        false ->
            {reply, replace_false, State}
    end;
handle_call(Request, _From, State) ->
    ?LOG_WARNING("undefined call", [Request]),
    {reply, {undefined_call,Request}, State}.

handle_cast({report_state, SvcReg, NameValue}, State) ->
    do_report_state(SvcReg, NameValue, true),
    {noreply, State};
handle_cast({notify_svcdef_update, Svcdef}, State) ->
    do_notify_svcdef_update(Svcdef),
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_WARNING("undefined cast", [Msg]),
    {noreply, State}.

%%服务在线测试
handle_info(check_online, State=#state{def=Def}) ->
    do_check_online(Def#svcdef.regname),
    erlang:send_after(?CHECK_ONLINE_DELAY,self(),check_online),
    {noreply, State};
%%服务超长离线测试
handle_info(check_offline, State=#state{def=Def}) ->
    do_check_offline(Def#svcdef.regname),
    erlang:send_after(?CHECK_OFFLINE_DELAY,self(),check_offline),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_WARNING("undefined info", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
do_register_service(SvcReg, SubID) ->
    case dbutil:read(dservice_state, SvcReg) of
        {atomic,[R=#dservice_state{subid=O_S}]} ->
            if
                SubID =/= O_S -> dbutil:write(dservice_state,R#dservice_state{subid=SubID});
                true -> ok
            end;
        _ -> dbutil:write(dservice_state,#dservice_state{svcreg=SvcReg,subid=SubID,update_time=now(),
                                          states=[{svcstat,<<"online">>,<<"false">>}]})
    end,
    do_report_state(SvcReg, [{svcstat,<<"online">>,<<"false">>}], true).

do_unregister_service(SvcReg) ->
    do_report_state(SvcReg, [{svcstat,<<"online">>,<<"false">>}], true),
    {atomic,ok} = dbutil:delete(dservice_state, SvcReg).

%% ------------------------------------------------------------------
%%服务在线测试
%%根据dservice_state表最后更新时间，测试服务实例的在线情况，
%%超过90秒没有更新认为不在线
do_check_online(RegName) ->
    {atomic, Records} = dbutil:match(dservice_state, {dservice_state,{svcreg,RegName,'_','_'},'_','_','_'}),
    [check_one_online(R) || R<-Records].
check_one_online(#dservice_state{svcreg=SvcReg,update_time=Time}) ->
    Tdiff = timer:now_diff(now(), Time)/1000000,
    if  Tdiff > 90 ->
            do_report_state(SvcReg,[{svcstat,<<"online">>,<<"false">>}],false);
        true -> ok
    end.
%% ------------------------------------------------------------------
%%服务超长离线测试
do_check_offline(RegName) ->
        {atomic, Records} = dbutil:match(dservice_state, {dservice_state,{svcreg,RegName,'_','_'},'_','_','_'}),
        [check_one_offline(R) || R<-Records].
    check_one_offline(R=#dservice_state{update_time=Time}) ->
        Tdiff = timer:now_diff(now(), Time)/1000000,
        if  
            Tdiff > 300 -> do_offline_without_unreg(R);
            true -> ok
        end.
    do_offline_without_unreg(R) ->
        case dbutil:get_config(dservice_offline_alertor) of
            {ok,Config} -> offline_alertor(R, Config);
            _ -> offline_alertor(R, undefined)
        end.
    offline_alertor(#dservice_state{svcreg=SvcReg=#svcreg{regname=RegName},update_time=Time}, F) ->
        Label = "service offline",
        %% Source = lists:flatten(io_lib:format("~p",[erlang:node()])),
        Source = lists:flatten(io_lib:format("dsf_service:~p",[RegName])),
        Host = binary_to_list(SvcReg#svcreg.host),
        DateTime = calendar:now_to_local_time(Time),
        offline_alertor_with_flag(SvcReg, Label, Source, Host, DateTime, F).
    offline_alertor_with_flag(SvcReg, Label, Source, Host, DateTime, undefined) ->
        %离线超过1周的服务,记录日志,向告警服务器发送告警
        ?LOG_WARNING(Label,Source,Host,"服务非注销离线",[{last_update,DateTime},{reg_server,SvcReg}]);
    offline_alertor_with_flag(SvcReg, Label, Source, Host, DateTime, once) ->
        ?LOG_WARNING(Label,Source,Host,"服务非注销离线",[{last_update,DateTime},{reg_server,SvcReg}]),
        {atomic,ok} = dbutil:delete(dservice_state, SvcReg);
    offline_alertor_with_flag(SvcReg, _Label, _Source, Host, DateTime, _) ->
        ?LOG_INFO("服务非注销离线",[{last_update,DateTime}, {host,Host}, {reg_server,SvcReg}]).

%% ------------------------------------------------------------------
%%更新上报的服务状态，并推送给订阅者
do_report_state(SvcReg=#svcreg{regname=RegName},NameValueList,Update) ->
    case replace_state(SvcReg, NameValueList, Update) of
        true ->
            {atomic, SubList} = dbutil:match({subscribe_map, '_',RegName}),
            Msg = {notify_svc_state, {SvcReg,NameValueList}},
            %%将参数中服务实例的状态推送给指定订阅者,订阅者不在线不做任何操作
            [send_notify(SubID, Msg, Update) || #subscribe_map{subid=SubID} <- SubList];
        false -> ok
    end.

%%更新dservice_static表中指定服务实例的状态记录
replace_state(SvcReg, List, Update) ->
    case dbutil:read(dservice_state,SvcReg) of
        {atomic,[]} -> false;
        {atomic,[R=#dservice_state{states=OldList}]} ->
            NewList = do_replace_state(OldList,List),
            Time = if Update=:=false -> R#dservice_state.update_time;
                      true  -> now()
            end,
            dbutil:write(dservice_state,R#dservice_state{update_time=Time,states=NewList}),
            true
    end.
do_replace_state(OldList,[]) ->
    OldList;
do_replace_state(OldList,[S=#svcstat{name=N}|List]) ->
    NewList = lists:keystore(N,2,OldList,S),
    do_replace_state(NewList,List).

%% ------------------------------------------------------------------
do_notify_svcdef_update(Svcdef=#svcdef{regname=RegName}) ->
    {atomic, SubList} = dbutil:match({subscribe_map, '_', RegName}),
    Msg = {notify_svcdef_update, Svcdef},
    [send_notify(SubID, Msg, true) || #subscribe_map{subid=SubID} <- SubList].


%% ------------------------------------------------------------------
send_notify(SubID, Msg, false) ->
    Name = {dsfclient,SubID},
    case global:whereis_name(Name) of
        undefined -> do_nothing;
        Pid ->
            L_N = node(),
            case node(Pid) of
                L_N -> Pid ! Msg;
                _ -> do_nothing
            end
    end;
send_notify(SubID, Msg, _) ->
    Name = {dsfclient,SubID},
    case global:whereis_name(Name) of
        undefined -> do_nothing;
        Pid ->
            Pid ! Msg
    end.
    
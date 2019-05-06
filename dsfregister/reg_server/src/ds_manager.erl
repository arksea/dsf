%%%-------------------------------------------------------------------
%%% $Date: 2015-08-13 09:48:39 +0800 (周四, 13 八月 2015) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(ds_manager).
-vsn("$Revision: 70196 $").
-behaviour(gen_server).

-include("define.hrl").
-include("reg_server.hrl").

-record(state, {}).

-define(SERVER,?MODULE).
-define(CHECK_OFFLINE_DELAY, 30000).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_svcdef/2, del_svcdef/1, update_svcdef_idl/2, update_svcdef/1]).



%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(StartArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).

add_svcdef(Svcdef, IDL) ->
    case dao:add_svcdef(Svcdef, IDL) of
        {atomic, ok} -> gen_server:cast(ds_manager, {notify_cluster_add_svcdef, Svcdef});
        {aborted, Reason} -> {error, Reason}
    end.

del_svcdef(RegName) ->
    case dao:delete_svcdef(RegName) of
        {atomic, ok} -> gen_server:cast(ds_manager, {notify_cluster_del_svcdef, RegName});
        {aborted, Reason} -> {error, Reason}
    end.

update_svcdef_idl(Svcdef, IDL) ->
    case dao:update_svcdef_idl(Svcdef, IDL) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

update_svcdef(Svcdef) ->
    gen_server:call(?SERVER, {update_svcdef, Svcdef}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_StartArgs) ->
    process_flag(trap_exit,true),


    pg2:create(ds_manager_group),
    pg2:join(ds_manager_group, self()),
    dbutil:foldl(fun load_dservice/2,ok,svcdef),
    check_invalid_online_dsfclient(),
    erlang:send_after(?CHECK_OFFLINE_DELAY,self(),check_offline),
    {ok,#state{}}.
load_dservice(Def=#svcdef{regname=DService},Acc0) ->
    dservice_sup:start_dservice_child(DService,{Def,[]}),
    Acc0.


handle_call({update_svcdef, Svcdef}, _From, State) ->
    case dao:update_svcdef(Svcdef) of
        {atomic, ok} ->
            dservice:notify_svcdef_update(Svcdef),
            {reply, ok, State};
        {aborted, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, undefined_call, State}.

handle_cast({notify_cluster_add_svcdef, Svcdef}, State) ->
    case pg2:get_members(ds_manager_group) of
        {error, R} -> {error, R};
        Members ->
            ?LOG_INFO("add_svcdef notify cluster", Members),
            case pg2:get_local_members(ds_manager_group) of
                {error, R} -> {error, R};
                LocalMembers ->
                    Nodes = Members -- LocalMembers,
                    %% [ gen_server:cast({ds_manager, Node}, {start_dservice_child, Svcdef}) || Node <- Nodes ],
                    [ gen_server:cast(Pid, {start_dservice_child, Svcdef}) || Pid <- Nodes ],
                    {noreply, State}
            end
    end;
handle_cast({notify_cluster_del_svcdef, RegName}, State) ->
    case pg2:get_members(ds_manager_group) of
        {error, R} -> {error, R};
        Members ->
            ?LOG_INFO("del_svcdef notify cluster", Members),
        
            case pg2:get_local_members(ds_manager_group) of
                {error, R} -> {error, R};
                LocalMembers ->
                    Nodes = Members -- LocalMembers,
                    [ gen_server:cast(Pid, {stop_dservice_child, RegName}) || Pid <- Nodes ],
                    {noreply, State}
            end
    end;
handle_cast({start_dservice_child, Svcdef=#svcdef{regname=RegName}}, State) ->
    ?LOG_INFO("ds_manager start_dservice_child", [Svcdef]),
    dservice_sup:start_dservice_child(RegName,{Svcdef,[]}),
    {noreply, State};
handle_cast({stop_dservice_child, RegName}, State) ->
    ?LOG_INFO("ds_manager stop_dservice_child", [RegName]),
    dservice_sup:stop_dservice_child(RegName),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_offline, State) ->
    check_offline_dsfclient(),
    erlang:send_after(?CHECK_OFFLINE_DELAY,self(),check_offline),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    pg2:leave(ds_manager_group, self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%注册服务器启动时检查所有online状态的dsf，并将其设置为offline
check_invalid_online_dsfclient() ->
    {atomic, List} = dbutil:match({dsfclient, '_', '_', '_', '_', '_', {online, '_'}}),
    ?LOG_INFO("check invalid online dsfclient", List),
    [check_one_online_sub(S)||S<-List].
check_one_online_sub(DsfClient=#dsfclient{subid=SubID}) ->
    Name = {dsfclient, SubID},
    case global:whereis_name(Name) of
        undefined ->
            ?LOG_INFO("not found the client,set it to offline", [DsfClient]),
            dbutil:write(dsfclient, DsfClient#dsfclient{time={offline,now()}});
        _ -> ok
    end.

%定时检查offline状态的dsfclient，删除超过300秒的记录
%离线超过300秒又重连上来的客户端通过dsf:renew_subscribe逻辑同步订阅列表
check_offline_dsfclient() ->
    {atomic, List} = dbutil:match({dsfclient, '_', '_', '_', '_', '_', {offline, '_'}}),
    [check_one_offline_sub(S)||S<-List].
check_one_offline_sub(DsfClient=#dsfclient{subid=SubID,time={_,Time}}) ->
    D = regsvc_misc:now_diff_sec(Time),
    if
        D>300 ->
            ?LOG_INFO("delete the timeout offline client",[DsfClient]),
            dao:delete_dsfclient(SubID);
       true ->
            Name = {dsfclient, SubID},
            case global:whereis_name(Name) of
                undefined -> ok;
                _ ->
                    ?LOG_INFO("found the client,set it to online", [DsfClient]),
                    dbutil:write(dsfclient, DsfClient#dsfclient{time={online,now()}})
            end
    end.



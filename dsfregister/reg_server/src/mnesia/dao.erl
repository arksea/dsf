-module(dao).

-export([delete_dsfclient/1,add_svcdef/2,delete_svcdef/1,
    update_svcdef_idl/2,update_svcdef/1,
    perf_stat/5]).

-include("define.hrl").
-include("reg_server.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MAX_SVC_DEF_COUNT, 1000).

delete_dsfclient(SubID) ->
    F = fun() ->
            ok = mnesia:delete(dsfclient,SubID, write),
            ok = mnesia:delete(subscribe_map,SubID, write),
            ok
        end,
    dbutil:mnesia_transaction(F).


delete_svcdef(RegName) ->
    dbutil:mnesia_transaction(fun() -> do_delete_svcdef(RegName) end).
do_delete_svcdef(RegName) ->
        Records = mnesia:match_object(#dservice_state{svcreg={svcreg,RegName,'_','_'},
                                                      update_time='_',
                                                      states='_'}),
        check_registered_svc(RegName, Records).
    check_registered_svc(RegName, []) ->
        ok = mnesia:delete(svcdef, RegName, write),
        ok = mnesia:delete(svcdef_idl, RegName, write),
        ok = dservice_sup:stop_dservice_child(RegName),
        ok;
    check_registered_svc(RegName, [#dservice_state{update_time=U}|Records]) ->
        D = regsvc_misc:now_diff_hour(U),
        if D < 12 -> mnesia:abort(has_registered_svc);
            true -> check_registered_svc(RegName, Records)
        end.

add_svcdef(Svcdef, IDL) ->
    dbutil:mnesia_transaction(fun() -> do_add_svcdef(Svcdef, IDL) end).
do_add_svcdef(Svcdef, IDL) ->
        Count = mnesia:table_info(svcdef, size),
        if  Count < ?MAX_SVC_DEF_COUNT -> add_not_exists(Svcdef, IDL);
            true -> mnesia:abort(svcdef_count_limit)
        end.
    add_not_exists(Svcdef = #svcdef{regname=RegName}, IDL) ->
        case mnesia:read(svcdef,RegName,read) of
            [] ->
                ok = mnesia:write(svcdef, Svcdef, write),
                ok = mnesia:write(svcdef_idl, #svcdef_idl{regname=RegName,idl=IDL}, write),
                ok = dservice_sup:start_dservice_child(RegName,{Svcdef,[]}),
                ok;
            [_] ->
                mnesia:abort(svcdef_exists)
        end.

%% ------------------------------------------------------------------
%%特殊修改：修改service define和idl
%%类名，版本不能修改: 注册名由类名，版本组合得出
%%重点检查类名，版本，以及是否有服务register
update_svcdef_idl(Svcdef=#svcdef{regname=RegName}, IDL) ->
    UpdateFun = fun() -> 
        ok = mnesia:write(svcdef, Svcdef, write),
        ok = mnesia:write(svcdef_idl, #svcdef_idl{regname=RegName,idl=IDL}, write)
    end,
    dbutil:mnesia_transaction(fun() -> update_svcdef_check(Svcdef, true, UpdateFun) end).
%% ------------------------------------------------------------------
%%修改service define描述以及扩展信息
update_svcdef(Svcdef) ->
    UpdateFun = fun() ->
        ok = mnesia:write(svcdef, Svcdef, write)
    end,
    dbutil:mnesia_transaction(fun() -> update_svcdef_check(Svcdef, false, UpdateFun) end).

%%类名，版本，不能修改
%%通信协议及IDL在有服务在线时不能修改
update_svcdef_check(N=#svcdef{regname=RegName}, ModifiedIDL, UpdateFun) ->
    case mnesia:read(svcdef,RegName,read) of
        [O] when O#svcdef.name =:= N#svcdef.name,
                 O#svcdef.version =:= N#svcdef.version ->
            Records = mnesia:match_object(#dservice_state{svcreg={svcreg,RegName,'_','_'},
                                                          update_time='_',
                                                          states='_'}),
            update_svcdef_check_registered(N, O, ModifiedIDL, UpdateFun, Records);
        [_O] -> mnesia:abort(attempt_to_modify_key);
        []  -> mnesia:abort(svcdef_not_exists)
    end.
    update_svcdef_check_registered(_N, _O, _MIDL, UpdateFun, []) ->
        UpdateFun();
    update_svcdef_check_registered(N, O, ModifiedIDL, UpdateFun, [#dservice_state{update_time=U}|Records]) ->
        D = regsvc_misc:now_diff_hour(U),
        if D < 12 -> update_svcdef_check_protocol(N,O,ModifiedIDL,UpdateFun);
             true -> update_svcdef_check_registered(N,O,ModifiedIDL,UpdateFun,Records)
        end.
    update_svcdef_check_protocol(#svcdef{props={_, NProps}},
                                 #svcdef{props={_, OProps}},_ModifiedIDL,UpdateFun) ->
        {value, {_, OV}} = lists:keysearch("protocol", 1, OProps),
        {value, {_, NV}} = lists:keysearch("protocol", 1, NProps),
        
        %% 20140626 modify : 业务要求，不检查IDL的修改
        if  OV =/= NV -> mnesia:abort(has_registered_svc);
                true -> UpdateFun()
%        if  OV =/= NV orelse ModifiedIDL -> mnesia:abort(has_registered_svc);
%                                    true -> UpdateFun()
        end.



perf_stat(K, R_T, L_T, D_R, D_C) ->
    dbutil:mnesia_transaction(fun() -> do_perf_stat(K, R_T, L_T, D_R, D_C) end).
do_perf_stat(K, R_T, L_T, D_R, D_C) ->
        case dbutil:read(dsconfig, K) of
            {atomic,[]} ->
                do_add_perf(K, R_T, L_T, D_R, D_C);
            {atomic,[C]} ->
                #dsconfig{config=OldProps} = C,
                do_replace_props(OldProps, K, R_T, L_T, D_R, D_C)
        end.
    do_add_perf(K, R_T, L_T, D_R, D_C) ->
        Perf = [{duration, R_T}, {request_count, D_R}, {cur_qps, D_R}, {cur_conn_count, D_C}, {last_update, L_T}],
        do_wirte_db(K, Perf).
    do_replace_props(OldProps, K, R_T, L_T, D_R, D_C) ->
        P1 = do_prop_replace_for_add(OldProps, request_count, D_R),
        P2 = lists:keystore(duration, 1, P1, {duration, R_T}),
        P3 = lists:keystore(cur_qps, 1, P2, {cur_qps, D_R}),
        P4 = lists:keystore(cur_conn_count, 1, P3, {cur_conn_count, D_C}),
        Props = lists:keystore(last_update, 1, P4, {last_update, L_T}),
        do_wirte_db(K, Props).
    do_wirte_db(K, Props) ->
        dbutil:write(dsconfig, {dsconfig, K, Props}).
        
do_prop_replace_for_add(Props, Name, Value) ->
    case lists:keyfind(Name, 1, Props) of
        false ->
            lists:keystore(Name, 1, Props, {Name, Value});
        {_, V} ->
            N_V = V + Value,
            lists:keystore(Name, 1, Props, {Name, N_V})
    end.
do_prop_replace_for_max(Props, Name, Value) ->
    case lists:keyfind(Name, 1, Props) of
        false ->
            lists:keystore(Name, 1, Props, {Name, Value});
        {_, V} ->
            if
                Value > V ->
                    lists:keystore(Name, 1, Props, {Name, Value});
                true ->
                    Props
            end
    end.


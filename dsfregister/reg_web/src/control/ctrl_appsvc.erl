-module(ctrl_appsvc).
-export([handle_request/3]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("reg_server.hrl").
-include("define.hrl").

handle_request('GET',[],_Args) ->
    query_svcdef();
handle_request('GET',[RegName],_Args) ->
    query_svc_state(RegName);
handle_request('POST',[],Args) ->
    add_appsvc(yaws_api:parse_post(Args));
handle_request('POST',[RegName],Args) ->
    update_appsvc(yaws_api:parse_post(Args),RegName);
handle_request('DELETE',[RegNameStr],_Args) ->
    delete_appsvc(RegNameStr).

query_svcdef() ->
    case dbutil:do_query(qlc:q([X||X<-mnesia:table(svcdef)])) of
        {atomic, Records} ->
            SvcdefList = svcdef_records_to_tuple(Records,[]),
            {ok,HTML} = view_svcdef_table:render([{svcdef_list,SvcdefList}]),
            {html,HTML};
        Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
    end.
svcdef_records_to_tuple([],List) ->
    List;
svcdef_records_to_tuple([R|Rest],List) ->
    {obj,Props} = R#svcdef.props,
    Fun = fun({K,V},Str) -> Str ++ K++" = "++binary_to_list(V)++"<br>" end,
    PropsStr = lists:foldl(Fun,"",Props),
    {ok, SCount} = webdao:get_subscribe_count(R#svcdef.regname),
    {ok, ICount} = webdao:get_svc_instance_count(R#svcdef.regname),
    T=[{regname,R#svcdef.regname},{name,R#svcdef.name},
                                  {version,R#svcdef.version},
                                  {props,PropsStr},
                                  {inst_count, ICount},
                                  {sub_count, SCount}],
    svcdef_records_to_tuple(Rest,[T|List]).

query_svc_state(RegNameStr) ->
        RegName = erlang:list_to_existing_atom(RegNameStr),
        query_dservice_state_record(RegName, RegNameStr).
    query_dservice_state_record(RegName, RegNameStr) ->
        case dbutil:match(#dservice_state{svcreg={svcreg,RegName,'_','_'},
                                          subid='_',
                                          update_time='_',
                                          states='_'}) of
            {atomic, Records} ->
                query_subscriber_record(RegName, RegNameStr, Records);
            Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
        end.
    query_subscriber_record(RegName, RegNameStr, DserviceRecords) ->
        case dbutil:do_query(qlc:q([Y|| X <- mnesia:table(subscribe_map),
                                        X#subscribe_map.regname =:= RegName,
                                        Y <- mnesia:table(dsfclient),
                                        X#subscribe_map.subid =:= Y#dsfclient.subid])) of
            {atomic, Records} ->
                query_svcdef_record(RegName, RegNameStr, DserviceRecords, Records);
            Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
        end.
    query_svcdef_record(RegName, RegnameStr, DserviceRecords,SubscriberRecords) ->
        case dbutil:do_query(qlc:q([X||X<- mnesia:table(svcdef),X#svcdef.regname =:= RegName])) of
            {atomic, [Record]} ->
                generate_record_from_record(RegName, RegnameStr, DserviceRecords, SubscriberRecords, Record);
            Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
        end.
    generate_record_from_record(RegName, RegNameStr, DserviceRecords, SubscriberRecords, Svcdef) ->
        DserviceList = svcstate_records_to_tuple(DserviceRecords,[]),
        SubscribeList = subscriber_records_to_tuple(SubscriberRecords,[]),

        RelyList = rely_service_record_to_tuple(RegName, DserviceRecords, []),
        BeRelyedList = berelyed_service_record_to_tuple(RegName, SubscriberRecords, []),

        {obj,Props} = Svcdef#svcdef.props,
        IDL = case dbutil:read(svcdef_idl, RegName) of
            {atomic, [#svcdef_idl{idl=T}]} -> T;
            {atomic, []} -> ""
        end,
        {ok,HTML} = view_svc_state_table:render([{regname, RegNameStr},
                                                 {name, Svcdef#svcdef.name},
                                                 {version, Svcdef#svcdef.version},
                                                 {description, Svcdef#svcdef.description},
                                                 {props, Props},
                                                 {state_list,DserviceList},
                                                 {subscribe_list, SubscribeList},
                                                 {rely_list, RelyList},
                                                 {berelyed_list, BeRelyedList},
                                                 {thrift,IDL}]),
        {html,HTML}.

svcstate_records_to_tuple([],List) ->
    List;
svcstate_records_to_tuple([R|Rest],List) ->
    V = R#dservice_state.svcreg,
    {{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(R#dservice_state.update_time),
    Time = lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,HH,MM,SS])),
    States = R#dservice_state.states,
    Fun = fun({svcstat,Key,Value},Str) -> Str ++ binary_to_list(Key)++" = "++binary_to_list(Value)++"<br>" end,
    StatesStr = lists:foldl(Fun,"",States),
    T=[{host,V#svcreg.host},
       {port,V#svcreg.port},
       {update_time,Time},
       {states,StatesStr}],
    svcstate_records_to_tuple(Rest,[T|List]).
subscriber_records_to_tuple([], List) ->
    List;
subscriber_records_to_tuple([R|Rest], List) ->
    {O, T} = R#dsfclient.time,
    {{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(T),
    {IP1, IP2, IP3, IP4} = R#dsfclient.ip,
    IP = lists:flatten(io_lib:format("~p.~p.~p.~p",[IP1, IP2, IP3, IP4])),
    Time = lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p",[Y,M,D,HH,MM,SS])),
    Tuple=[{name,R#dsfclient.name},
        {host,IP},
        {port, R#dsfclient.port},
        {online,O},
        {update_time,Time}],
    subscriber_records_to_tuple(Rest,[Tuple|List]).
rely_service_record_to_tuple(_RegName, [], List) ->
    sets:to_list(sets:from_list(List));
rely_service_record_to_tuple(RegName, [R|Rest], List) ->
    S = R#dservice_state.subid,
    case dbutil:do_query(qlc:q([X || X<- mnesia:table(subscribe_map),X#subscribe_map.subid =:= S])) of
        {atomic, Records} ->
            L = [[{name, atom_to_list(Y#subscribe_map.regname)}] || Y <- Records, Y#subscribe_map.regname =/= RegName],
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
            L = [[{name, atom_to_list(Y#svcreg.regname)}] || Y <- Records, Y#svcreg.regname =/= RegName],
            berelyed_service_record_to_tuple(RegName, Rest, L ++ List);
        _ ->
            berelyed_service_record_to_tuple(RegName, Rest, List)
    end.

add_appsvc(Args) ->
    {"submit", Submit}  = lists:keyfind("submit",1,Args),
    case Submit of
        "cancel" -> {redirect_local, "/index.yaws/appsvc"};
        _ ->
            Action = fun(Def,IDL) -> ds_manager:add_svcdef(Def,IDL) end,
            {"name", Name}       = lists:keyfind("name",1,Args),
            {"version", Ver}     = lists:keyfind("version",1,Args),
            RegNameStr = Name++" v"++Ver,
            do_update_appsvc(Args, RegNameStr, Action, "")
    end.

delete_appsvc(RegNameStr) ->
    RegName = regsvc_misc:regname_to_exists_atom(RegNameStr),
    ds_manager:del_svcdef(RegName).

update_appsvc(Args, RegNameStr) ->
        {"submit", Submit}  = lists:keyfind("submit",1,Args),
        case Submit of
            "cancel" ->
                {redirect_local, "/index.yaws/appsvc/"++RegNameStr};
            "delete" ->
                delete_appsvc(RegNameStr),
                {redirect_local, "/index.yaws/appsvc"};
            "modify" ->
                {redirect_local, "/index.yaws/editor/appsvc/"++RegNameStr};
            _ ->
                Action = fun(Def,IDL) -> ds_manager:update_svcdef_idl(Def,IDL) end,
                do_update_appsvc(Args, RegNameStr, Action, RegNameStr)
        end.

do_update_appsvc(Args, RegNameStr, Action, ActionPath) ->
    {"thrift", Thrift}   = lists:keyfind("thrift",1,Args),
    {"name", Name}       = lists:keyfind("name",1,Args),
    {"version", Ver}     = lists:keyfind("version",1,Args),
    {"protocol", Protocol} = lists:keyfind("protocol",1,Args),
    {"description", Desc} = lists:keyfind("description",1,Args),
    {"route", Route} = lists:keyfind("route",1,Args),
    {"failed", Failed} = lists:keyfind("failed",1,Args),
    {"timeout", Timeout} = lists:keyfind("timeout",1,Args),
    RegName = erlang:list_to_atom(RegNameStr),
    Props = [{"protocol",list_to_binary(Protocol)},
             {"route_strategy",list_to_binary(Route)},
             {"fail_strategy",list_to_binary(Failed)},
             {"timeout",list_to_binary(Timeout)}],
    Svcdef = #svcdef{regname=RegName,
                     name=list_to_binary(Name),
                     version=list_to_binary(Ver),
                     description=list_to_binary(Desc),
                     props = {obj,Props}},
    case compile_and_save_appsvc(RegNameStr, Svcdef, Thrift, Action) of
        ok -> {redirect_local, "/index.yaws/appsvc/"++RegNameStr};
        {error, Reason} ->
            {ok,HTML} = view_add_appsvc_form:render([{action_path, ActionPath},
                                                     {name, Name},
                                                     {version, Ver},
                                                     {thrift, Thrift},
                                                     {description,Desc},
                                                     {compile_info,"编译失败：<br/>"++Reason},
                                                     {props, Props}]),
            {html, HTML}
    end.
compile_and_save_appsvc(RegNameStr, Svcdef, Thrift, Action) ->
    case make_thrift(RegNameStr, "java", Thrift) of
        ok -> Action(Svcdef, Thrift);
        E -> E
    end.
    

%%  do(qlc:q([X#shop.item || X <- mnesia:table(shop),
%%                           X#shop.quantity < 250,
%%                           Y <- mnesia:table(cost),
%%                           X#shop.item =:= Y#cost.name,
%%                           Y#cost.price < 2])).

make_thrift(RegNameStr, Language, IDL) ->
    Name = binary_to_list(base64:encode(RegNameStr)),
    Path = code:root_dir()++"/www/idl/"++Name,
    write_idl_file(Path,Name,Language,IDL).
write_idl_file(Path,Name,Language,IDL) ->
    File = Path++"/thrift_idl",
    ok = filelib:ensure_dir(File),
    {ok, IO} = file:open(File, [raw,write]),
    ok = file:write(IO,IDL),
    ok = file:close(IO),
    compile_thrift(Path,Name,Language,IDL).
compile_thrift(Path,_Name,Language,_IDL) ->
    Thrift = code:priv_dir("reg_web")++"/thrift",
    Cmd = Thrift++" --gen "++Language++" -o "++Path++" "++Path++"/thrift_idl 2<&1",
    case os:cmd(Cmd) of
       [] -> ok;
       Err -> {error, Err}
    end.


%do_get_file(FilePath, Sock) ->
%    Tar = "./temp/upload.tar.gz",
%    ok = do_mfa(filelib,ensure_dir,[Tar],Sock),
%    ok = filelib:ensure_dir(Tar),
%    Ret = do_mfa(os,cmd,["tar -cvzf "++Tar++" "++FilePath],Sock),
%    io:format("tar files:~p~n",[Ret]),
%    io:format("open local file to write: ~p~n",[Tar]),
%    {ok, IoDevice} = file:open(Tar, [raw,write,binary]),
%    io:format("open remote file to read: ~p~n",[Tar]),
%    {ok,RemoteIoDevice} = do_mfa(file,open,[Tar, [raw,read,binary]],Sock),
%    {ok, #file_info{size=Size}} = do_mfa(file,read_file_info,[Tar],Sock),
%    io:format("file size = ~p~n",[Size]),
%    io:format("rcv packs~n"),
%    rcv_pack(Size,IoDevice,RemoteIoDevice,Sock) ,
%    ok = file:close(IoDevice),
%    ok = do_mfa(file,close,[RemoteIoDevice],Sock),
%    {ok, #file_info{size=RcvSize}} = file:read_file_info(Tar),
%    io:format("rcv packs finish,size=~p~n",[RcvSize]),
%    ok = do_mfa(file,delete,[Tar],Sock).

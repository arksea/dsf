-module(ctrl_editor).
-export([handle_request/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("reg_server.hrl").

handle_request('GET',["appsvc"],_Args) ->
    IDL =   "namespace java com.baidu91.svc\n"++
            "namespace csharp com.baidu91.svc\n"++
            "namespace cpp com.baidu91.svc\n\n"++
            "const i32 NUM = 2\n\n"++
            "service Service1{\n"++
            "    void ping(),\n"++
            "    i64  query(1:string s)\n"++
            "}\n",
    {ok, Html} = view_add_appsvc_form:render([{thrift,IDL},
                                              {action_path,""},
                                              {is_modify_exists_svc, ""},
                                              {name,""},
                                              {version,"1.0"},
                                              {description,""},
                                              {appsvc_exists,""},
                                              {compile_info,""},
                                              {props,[{route_strategy,"roundrobin"},
                                                      {fail_strategy,"failfast"},
                                                      {protocol,"framed-binary"},
                                                      {timeout,"10000"}]}
                                              ]),
    {html, Html};
handle_request('GET',["appsvc",RegNameStr],_Args) ->
    RegName = list_to_atom(RegNameStr),
    case dbutil:do_query(qlc:q([X||X<- mnesia:table(svcdef),X#svcdef.regname =:= RegName])) of
        {atomic, [Svcdef]} ->
            {obj,Props} = Svcdef#svcdef.props,
            IDL = case dbutil:read(svcdef_idl, RegName) of
                {atomic, [#svcdef_idl{idl=T}]} -> T;
                {atomic, []} -> ""
            end,
            {ok,HTML} = view_add_appsvc_form:render([{thrift,IDL},
                                                     {action_path, RegNameStr},
                                                     {is_modify_exists_svc, "readonly=\"readonly\""},
                                                     {name, Svcdef#svcdef.name},
                                                     {version, Svcdef#svcdef.version},
                                                     {description,Svcdef#svcdef.description},
                                                     {compile_info,""},
                                                     {props, Props}]),
            {html, HTML};
        Error -> {html,lists:flatten(io_lib:format("查询失败:~p",[Error]))}
    end.



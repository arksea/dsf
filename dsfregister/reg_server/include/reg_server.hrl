-vsn("$Revision:1$").

-include("jsonrfc.hrl").

-record(tcpmsg,{name::jsonstr(),value::jsonobj()}).

-record(svcreg,{regname::jsonstr()|atom(),
                host::jsonstr(),
                port::integer()
                }).

-record(svcdef,{regname::jsonstr() | atom(),
                name::jsonstr(),
                version::jsonstr(),
                description::jsonstr(),
                props::jsonobj()}).

-record(svcdef_idl,{regname::jsonstr() | atom(),
                idl::jsonstr()
                }).

-record(svc_subscribe,{subid::integer(),
                       dsf::pid(),
                       svc::[jsonstr()]}).

-record(svc_report,{svc::jsonobj(),
                    name::jsonstr(),
                    value::jsonstr()}).

-record(svcaddr,{host::jsonstr(), port::integer()}).
-record(svcstat,{name::jsonstr(),value::jsonstr()}).
-record(subscribe_result,{svcdef::#svcdef{},svclist::[{#svcaddr{},[#svcstat{}]}]}).

 %客户端状态表
-record(dsfclient, {subid::jsonstr(), %订阅号
                    name::jsonstr(),
                    ver::jsonstr(),
                    ip::erlang:ip_address(),
                    port::integer(),
                    time::{online,erlang:timestamp()}|{offline,erlang:timestamp()} %订阅者在线情况,与登录时间
                    }).

%服务订阅者映射表，多对多
-record(subscribe_map, {subid::jsonstr(), regname::atom()}).

%dservice_state, [set]), %{{host::binary(),port::integer()},update_time::time(),[{name::binary(),value::binary()}]}

-record(dservice_state, {
    svcreg::#svcreg{},
    subid::jsonstr(),
    update_time::erlang:timestamp(),
    states::[#svcstat{}]}).

-record(set_svc_state, {
    regname::jsonstr(),
    addr::#svcaddr{},
    states::[#svcstat{}]}).

%
%-record(perf_prop, {name::string(), value::atom() | integer() | string()}).
%
%-record(perf_stat, {date::erlang:date(), props::[#perf_prop{}]}).
%

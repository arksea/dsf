-module(webdao).
-export([get_svc_instance_count/1,get_subscribe_count/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("reg_server.hrl").

get_svc_instance_count(RegName) when is_atom(RegName) ->
    case dbutil:match(#dservice_state{svcreg={svcreg,RegName,'_','_'},
                                      subid='_',
                                      update_time='_',
                                      states='_'}) of
        {atomic, Records} -> {ok, length(Records)};
        Error -> {error, Error}
    end.

get_subscribe_count(RegName) when is_atom(RegName) ->
    case dbutil:do_query(qlc:q([Y|| X <- mnesia:table(subscribe_map),
                                    X#subscribe_map.regname =:= RegName,
                                    Y <- mnesia:table(dsfclient),
                                    X#subscribe_map.subid =:= Y#dsfclient.subid])) of
        {atomic, Records} -> {ok, length(Records)};
        Error -> {error, Error}
    end.
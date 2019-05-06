%%%-------------------------------------------------------------------
%%% $Date: 2014-04-04 19:34:50 +0800 (周五, 04 四月 2014) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(reg_server_app).

-behaviour(application).

-include("define.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dbutil:init(),

    reg_server_sup:start_link().

stop(_State) ->
    {{Y,M,D},{HH,MM,_}} = calendar:now_to_local_time(now()),
    FileName = lists:flatten(io_lib:format("./log/dump_db_~p-~p-~p-~p-~p",[Y,M,D,HH,MM])),
    mnesia:dump_to_textfile(FileName),
    ?LOG_INFO("stop in reg_server_app", [file, FileName]),
    ok.

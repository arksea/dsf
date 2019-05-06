%%%-------------------------------------------------------------------
%%% $Date: 2013-11-22 09:05:55 +0800 (周五, 22 十一月 2013) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(dsf_base_app).

-behaviour(application).

-include("define.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?LOG_INFO("dsf base start", []),
    ark_alarm_handler:swap_handler(),
    dsf_base_sup:start_link().

stop(_State) ->
    ?LOG_INFO("dsf base stop", []),
    ok.

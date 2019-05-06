%%%-------------------------------------------------------------------
%%% $Date: 2012-03-02 01:37:06 +0800 (�? 2012-03-02) $
%%% $Author: xiaohx $
%%%-------------------------------------------------------------------
-module(reg_web_app).

-vsn("$Revision: 74 $").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    reg_web_sup:start_link().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%%% $Date: 2012-11-29 11:31:35 +0800 (周四, 29 十一月 2012) $
%%% $Author: xiaohx $
%%%-------------------------------------------------------------------
-module(reg_web_sup).

-behaviour(supervisor).
-vsn("$Revision: 492 $").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    ets:new(view_module_map,[named_table,public]),
    {ok, { {one_for_one, 10, 300}, []} }.


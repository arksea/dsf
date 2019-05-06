%%%-------------------------------------------------------------------
%%% $Date: 2014-01-27 17:23:53 +0800 (周一, 27 一月 2014) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(dsf_base_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Alertor = ?CHILD(alertor, worker),
    {ok, { {one_for_one, 10, 10}, [Alertor]} }.

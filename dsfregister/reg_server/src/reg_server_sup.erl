%%%-------------------------------------------------------------------
%%% $Date: 2015-08-13 09:48:39 +0800 (周四, 13 八月 2015) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(reg_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("define.hrl").

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

    ?LOG_INFO("global sync strat", []),
    {_, S, M_S} = os:timestamp(),
    Sync = global:sync(),
    {_, NS, M_NS} = os:timestamp(),
    T = (NS - S)*100000 + M_NS - M_S,
    ?LOG_INFO("global sync state", [Sync, T]),

    DserviceSup = {dservice_sup, {dservice_sup,start_link,[nil]},
                                 permanent,5000,supervisor,[dservice_sup]},
    DserviceManager = {ds_manager,{ds_manager,start_link,[nil]},
                                        permanent,5000,worker,[ds_manager]},
    DsfSup = {dsf_sup, {dsf_sup,start_link,[nil]},
                       permanent,5000,supervisor,[dsf_sup]},
    DsfListener = init_dsf_listener(),
    PerfStat = {perf_stat,{perf_stat,start_link,[nil]},
                                    permanent,5000,worker,[ds_manager]},
    {ok, { {one_for_one, 10, 10}, [DserviceSup,DserviceManager,DsfSup,DsfListener,PerfStat]} }.

init_dsf_listener() ->
    {{_,Port},{_,AcceptorCount}} = regsvc_misc:get_env(dsf_listener,
                                                     {{port, 9002},{acceptor_count,5}}),
    ListenerArgs = [{0,0,0,0}, Port, [binary],
                    {dsf_sup, on_listener_startup, []},
                    {dsf_sup, on_listener_shutdown, []},
                    {dsf_sup, on_connect, []}, AcceptorCount, dsf],
    {dsf_listener_sup,{tcp_listener_sup,start_link,ListenerArgs},
     permanent,5000,supervisor,[tcp_listener_sup]}.

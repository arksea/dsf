%%%-------------------------------------------------------------------
%%% $Date: 2014-04-09 17:41:41 +0800 (周三, 09 四月 2014) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(dservice_sup).

-vsn("$Revision: 62186 $").

-behaviour(supervisor).

%% API
-export([start_link/1, 
        start_dservice_child/2,
        stop_dservice_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(ServerName,I,Args), {ServerName,{I,start_link,Args},permanent,5000,worker,[I]}).

-include("define.hrl").
-include_lib("stdlib/include/qlc.hrl").
%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

start_dservice_child(ServerName, Args) ->
    ChildSpec = ?CHILD(ServerName,dservice,[ServerName, Args]),
    case supervisor:start_child(dservice_sup,ChildSpec) of
        {error,Error} -> 
            ?LOG_ERROR("start dservice failed",[Error]),
            {error,Error};
        _ -> ok
    end.


stop_dservice_child(ServerName) ->
    supervisor:terminate_child(dservice_sup,ServerName),
    supervisor:delete_child(dservice_sup,ServerName),
    ok.



%restart_child(ServerName, Args) ->
%    case whereis(ServerName) of
%        undefined ->
%            ?LOG_WARNING("restart dservice warning, the service not exists",[ServerName]),
%            ChildSpec = ?CHILD(ServerName,dservice,[ServerName, Args]),
%            supervisor:start_child(dservice_sup,ChildSpec);
%        _Pid ->
%            supervisor:terminate_child(dservice_sup,ServerName),
%            supervisor:restart_child(dservice_sup,ServerName)
%    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    {ok, { {one_for_one, 10, 10},[]} }.



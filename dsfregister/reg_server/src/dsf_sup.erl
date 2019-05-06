%%%-------------------------------------------------------------------
%%% $Date: 2014-05-20 15:22:46 +0800 (周二, 20 五月 2014) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(dsf_sup).
-vsn("$Revision: 62581 $").
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([on_listener_startup/2,on_listener_shutdown/2,on_connect/1]).

%% Supervisor callbacks
-export([init/1]).
-include ("define.hrl").
-define(SERVER, ?MODULE).

start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

init(StartArgs) ->
    Child = {dsf,{dsf,start_link,[StartArgs]},
                 temporary,5000,worker,[dsf]},
    {ok,{{simple_one_for_one,10,10}, [Child]}}.

%%====================================================================
%% API
%%====================================================================
on_listener_startup(_IPAddress, _Port) ->
    ok.

on_listener_shutdown(_IPAddress, _Port) ->
    ok.

on_connect(Sock) ->
    try
        {ok, _} = inet:peername(Sock),
        {ok, Child} = supervisor:start_child(dsf_sup, []),
        ok = gen_tcp:controlling_process(Sock, Child),
        Child ! {go, Sock},
        ok
    catch
        error:{badmatch, {error, enotconn}} ->
            gen_tcp:close(Sock),
            ok;
        _T:E ->
            ?LOG_WARNING("Handshake failed with Exception",[E]),
            gen_tcp:close(Sock),
            ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================



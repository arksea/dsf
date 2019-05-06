%%%-------------------------------------------------------------------
%%% $Date: 2013-11-22 09:05:55 +0800 (周五, 22 十一月 2013) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(gen_server).
-vsn("$Revision: 59596 $").
-behaviour(gen_server).
%-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(StartArgs) ->
    ServerName = ?MODULE,
    gen_server:start_link({local, ServerName}, ?MODULE, StartArgs, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(StartArgs) ->
    State = [],
    {ok,State}.

handle_call(_Request, _From, State) ->
    {reply, undefined_call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

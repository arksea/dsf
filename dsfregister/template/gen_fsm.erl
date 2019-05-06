%%%-------------------------------------------------------------------
%%% $Date: 2013-11-22 09:05:55 +0800 (周五, 22 十一月 2013) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(_MODULE_NAME_).
-vsn("$Revision: 59596 $").
-behaviour(gen_fsm).

-export([start_link/1]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-record(state, {}).

-include("define.hrl").
-define(SERVER, ?MODULE).

start_link(StartArgs) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, StartArgs, []).

%%====================================================================
%% API
%%====================================================================

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init(_Args) ->
    {ok,stoped,#state{}}.
    %{ok,StateName,StateData}
    %{ok,StateName,StateData,Timeout}.
    %{ok,StateName,StateData,hibernate}.
    %{stop,Reason}.
    %ignore.
%
%StateName(Event, StateData) ->
%    {next_state,NextStateName,NewStateData}.
%    %{next_state,NextStateName,NewStateData}.
%    %{next_state,NextStateName,NewStateData,Timeout}.
%    %{next_state,NextStateName,NewStateData,hibernate}.
%    %{stop,Reason,NewStateData}.
%
handle_event(_Event, StateName, StateData) ->
    {next_state,StateName,StateData}.
    %{next_state,NextStateName,NewStateData}.
    %{next_state,NextStateName,NewStateData,Timeout}.
    %{next_state,NextStateName,NewStateData,hibernate}.
    %{stop,Reason,NewStateData}.

%StateName(Event, From, StateData) ->
%    {reply,Reply,NextStateName,NewStateData}.
%    %{reply,Reply,NextStateName,NewStateData}.
%    %{reply,Reply,NextStateName,NewStateData,Timeout}.
%    %{reply,Reply,NextStateName,NewStateData,hibernate}.
%    %{next_state,NextStateName,NewStateData}.
%    %{next_state,NextStateName,NewStateData,Timeout}.
%    %{next_state,NextStateName,NewStateData,hibernate}.
%    %{stop,Reason,Reply,NewStateData} | {stop,Reason,NewStateData}.
%
%
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply,ok,StateName,StateData}.
    %{reply,Reply,NextStateName,NewStateData}.
    %{reply,Reply,NextStateName,NewStateData,Timeout}.
    %{reply,Reply,NextStateName,NewStateData,hibernate}.
    %{next_state,NextStateName,NewStateData}.
    %{next_state,NextStateName,NewStateData,Timeout}.
    %{next_state,NextStateName,NewStateData,hibernate}.
    %{stop,Reason,Reply,NewStateData} | {stop,Reason,NewStateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state,StateName,StateData}.
    %{next_state,NextStateName,NewStateData}.
    %{next_state,NextStateName,NewStateData,Timeout}.
    %{next_state,NextStateName,NewStateData,hibernate}.
    %{stop,Reason,NewStateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%format_status(_Opt, [_PDict, _StateData]) ->
%    Status.

%%%--------------------------------------------------------------------
%%%% Internal functions
%%%--------------------------------------------------------------------

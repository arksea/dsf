%%%-------------------------------------------------------------------
%%% $Date: 2013-11-22 09:05:55 +0800 (周五, 22 十一月 2013) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(_MODULE_NAME_).
-vsn("$Revision: 59596 $").
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-include("define.hrl").


%%====================================================================
%% API
%%====================================================================

%%====================================================================
%% gen_event callbacks
%%====================================================================
init(_Args) ->
    {ok,#state{}}.
%   {ok,State}.
%   {ok,State,hibernate}.
%   {error,Reason}.

handle_event(_Event, State) ->
    {ok,State}.
%   {ok,NewState}.
%   {ok,NewState,hibernate}.
%   {swap_handler,Args1,NewState,Handler2,Args2}.
%   remove_handler.

handle_call(Request, State) ->
    {ok,undefined_call,State}.
%   {ok,Reply,NewState}.
%   {ok,Reply,NewState,hibernate}.
%   {swap_handler,Reply,Args1,NewState,Handler2,Args2}.
%   {remove_handler, Reply}.

handle_info(_Info, State) ->
    {ok,State}.
%   {ok,NewState}.
%   {ok,NewState,hibernate}.
%   {swap_handler,Args1,NewState,Handler2,Args2}.
%   remove_handler.

terminate(_Arg, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%format_status(_Opt, [_PDict, _StateData]) ->
%    Status.

%%%--------------------------------------------------------------------
%%%% Internal functions
%%%--------------------------------------------------------------------

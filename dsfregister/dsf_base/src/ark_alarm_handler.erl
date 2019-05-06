-module(ark_alarm_handler).
-behaviour(gen_event).
-vsn("$Revision: 74 $").

-export([swap_handler/0]).
-export([init/1,handle_event/2,handle_call/2,
         handle_info/2,terminate/2,code_change/3]).

-include("alarm_types.hrl").

-record(state, {source, host}).

swap_handler() ->
    gen_event:swap_handler(alarm_handler,
                           {alarm_handler, swap},
                           {ark_alarm_handler, arg}).

init(_Args) ->
    Source=atom_to_list(erlang:node()),
    {ok, HostName} = inet:gethostname(),
    {ok, {A1,A2,A3,A4}} = inet:getaddr(HostName, inet),
    HostAddr = lists:flatten(io_lib:format("~p.~p.~p.~p",[A1,A2,A3,A4])),
    {ok, #state{source=Source, host=HostAddr}}.

handle_event({set_alarm, {log_info,Msg,Details,Module,Line}}, State) ->
    error_logger:info_msg("      ~s    @(~p:~p)~n      ~p~n",
                          [Msg,Module,Line,Details]),
    {ok, State};
handle_event({set_alarm, {log_resume,Label,Module,Line}}, State=#state{source=Source,host=Host}) ->
    error_logger:info_msg("      resume alarm: ~s,~s,~s    @(~p:~p)~n",
                          [Label,Source,Host,Module,Line]),
    alertor:resume(Label,Source,Host),
    {ok, State};
handle_event({set_alarm, {log_resume,Label,Source,Host,Module,Line}}, State) ->
    error_logger:info_msg("      resume alarm: ~s,~s,~s    @(~p:~p)~n",
                          [Label,Source,Host,Module,Line]),
    alertor:resume(Label,Source,Host),
    {ok, State};
handle_event({set_alarm, {log_warning,Msg,Details,Module,Line}}, State=#state{source=Source,host=Host}) ->
    error_logger:warning_msg("      ~s    @(~p:~p)~n      ~p~n",
                             [Msg,Module,Line,Details]),
    Label = lists:flatten(io_lib:format("~p:~p",[Module,Line])),
    DStr = lists:flatten(io_lib:format("~p",[Details])),
    alert_alarm(?alarm_AlarmLevel_WARNING,Label,Source,Host,Msg,DStr),
    {ok, State};
handle_event({set_alarm, {log_warning,Label,Msg,Details,Module,Line}}, State=#state{source=Source,host=Host}) ->
    error_logger:warning_msg("      ~s    @(~p:~p)~n      ~p~n",
                           [Msg,Module,Line,Details]),
    DStr = lists:flatten(io_lib:format("~p",[Details])),
    alert_alarm(?alarm_AlarmLevel_WARNING,Label,Source,Host,Msg,DStr),
    {ok, State};
handle_event({set_alarm, {log_warning,Label,Source,Host,Msg,Details,Module,Line}}, State) ->
    error_logger:warning_msg("      ~s    @(~p:~p)~n      ~p~n",
                           [Msg,Module,Line,Details]),
    DStr = lists:flatten(io_lib:format("~p",[Details])),
    alert_alarm(?alarm_AlarmLevel_WARNING,Label,Source,Host,Msg,DStr),
    {ok, State};
handle_event({set_alarm, {log_error,Msg,Details,Module,Line,Stack}}, State=#state{source=Source,host=Host}) ->
    error_logger:error_msg("      ~s    @(~p:~p)~n      ~p~n      ~p~n",
                           [Msg,Module,Line,Details,Stack]),
    Label = lists:flatten(io_lib:format("~p:~p",[Module,Line])),
    DStr = lists:flatten(io_lib:format("~p~n      ~p",[Details, Stack])),
    alert_alarm(?alarm_AlarmLevel_WARNING,Label,Source,Host,Msg,DStr),
    {ok, State};
handle_event({set_alarm, {log_error,Label,Msg,Details,Module,Line,Stack}}, State=#state{source=Source,host=Host}) ->
    error_logger:error_msg("      ~s    @(~p:~p)~n      ~p~n      ~p~n",
                           [Msg,Module,Line,Details,Stack]),
    DStr = lists:flatten(io_lib:format("~p~n      ~p",[Details, Stack])),
    alert_alarm(?alarm_AlarmLevel_WARNING,Label,Source,Host,Msg,DStr),
    {ok, State};
handle_event({set_alarm, {log_error,Label,Source,Host,Msg,Details,Module,Line,Stack}}, State) ->
    error_logger:error_msg("      ~s    @(~p:~p)~n      ~p~n      ~p~n",
                           [Msg,Module,Line,Details,Stack]),
    DStr = lists:flatten(io_lib:format("~p~n      ~p",[Details, Stack])),
    alert_alarm(?alarm_AlarmLevel_ERROR, Label, Source, Host, Msg, DStr),
    {ok, State};
handle_event(Event, State) ->
    alarm_handler:handle_event(Event, State).

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
alert_alarm(Level, Label, Source, Host, Msg, Details) ->
    alertor:alert(Label, Source, Host, Level, Msg, Details).

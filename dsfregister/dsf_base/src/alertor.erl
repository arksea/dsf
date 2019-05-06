%%%-------------------------------------------------------------------
%%% $Date: 2013-11-22 09:05:55 +0800 (周五, 22 十一月 2013) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-module(alertor).
-vsn("$Revision: 59596 $").
-behaviour(gen_server).

-include("config.hrl").
-include("alarm_types.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(state, {client=offline,
                index=1, %当前使用的Alertor Server地址索引
                delay=2000, %重连延时
                reset_ref
                }).
-define(RECONN_DELAY_MAX, 30000).


%% use local log replace define.hrl
%% These avoided call itself for loop
-define(LOG_MSG(Msg),
    error_logger:info_msg("      ~s    @(~p:~p)~n",
                          [Msg,?MODULE,?LINE])).
-define(LOG_INFO(Msg, Details),
    error_logger:info_msg("      ~s    @(~p:~p)~n      ~p~n",
                          [Msg,?MODULE,?LINE,Details])).
-define(LOG_WARNING(Msg, Details),
    error_logger:warning_msg("      ~s    @(~p:~p)~n      ~p~n      ~p~n",
                             [Msg,?MODULE,?LINE,Details,erlang:get_stacktrace()])).
-define(LOG_ERROR(Msg, Details),
    error_logger:error_msg("      ~s    @(~p:~p)~n      ~p~n      ~p~n",
                           [Msg,?MODULE,?LINE,Details,erlang:get_stacktrace()])).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([alert/1, alert/4, alert/6, resume/1, resume/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    ServerName = ?MODULE,
    gen_server:start_link({local, ServerName}, ?MODULE, nil, []).

alert(Label, Level, Summary, Detail) when is_list(Label),
                                          is_integer(Level),
                                          is_list(Summary),
                                          is_list(Detail) ->
    Source=atom_to_list(erlang:node()),
    {ok, HostName} = inet:gethostname(),
    {ok, {A1,A2,A3,A4}} = inet:getaddr(HostName, inet),
    HostAddr = lists:flatten([A1,".",A2,".",A3,".",A4]),
    alert(Label, Source, HostAddr, Level, Summary, Detail).

alert(Label, Source, Host, Level, Summary, Detail) when is_list(Label),
                                                        is_list(Source),
                                                        is_list(Host),
                                                        is_integer(Level),
                                                        is_list(Summary),
                                                        is_list(Detail) ->
    {Megasecs, Secs, MicroSecs} = erlang:now(),
    TimeStamp = (Megasecs * 1000000 + Secs) * 1000 + MicroSecs div 1000,
    S = lists:flatten(io_lib:format("~48s", [Summary])),
    Key = #alarmKey{label=Label, source=Source, host=Host},
    Alarm = #alarm{key=Key,
                   level=Level,
                   summary=S,
                   timestamp=TimeStamp,
                   detail=Detail},
    alert(Alarm).

alert(Alarm=#alarm{}) ->
    gen_server:cast(?MODULE, {thrift_notify,{alert, [Alarm]}}).

resume(Label, Source, Host) ->
    Key = #alarmKey{label=Label, source=Source, host=Host},
    gen_server:cast(?MODULE, {thrift_notify,{resume, [Key]}}).

resume(Key=#alarmKey{}) ->
    gen_server:cast(?MODULE, {thrift_notify,{resume, [Key]}}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(_StartArgs) ->
    State = connect_to_alertor_server(#state{}),
    {ok,State}.

handle_call(_Request, _From, State) ->
    {reply, undefined_call, State}.


handle_cast({thrift_notify, {Method,Args}}, State=#state{client=offline}) ->
    ?LOG_WARNING("thrift_notify offline", [Method,Args]),
    {noreply, State};
handle_cast({thrift_notify, {Method,Args}}, State=#state{client=Client, delay=Delay}) ->
    try
        {ClientS1, {ok, ok}} = thrift_client:call(Client, Method, Args),
        {noreply, State#state{client=ClientS1}}
    catch
        Throw:{ClientS2, Ex} ->
            erlang:send_after(Delay, self(), reconnect),
            ?LOG_ERROR("put alarm to server failed",[{Throw, Ex},Args]),
            thrift_client:close(ClientS2),
            {noreply, State#state{client=offline, delay=backoff(Delay)}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnect, State) ->
   NewState = connect_to_alertor_server(State),
   {noreply, NewState};
handle_info(reset_delay, State) ->
    ?LOG_INFO("reset connect delay",[]),
    {noreply, State#state{reset_ref=undefined, delay=2000}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{client=offline}) ->
    ok;
terminate(_Reason, _State=#state{client=Client}) ->
    thrift_client:close(Client),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_config(Name, Default) ->
    case do_query(qlc:q([X||X<-mnesia:table(dsconfig),X#dsconfig.name =:= Name])) of
        {atomic, [{_,_,Config}]} -> Config;
        _Other -> Default
    end.

do_query(Q) ->
    Fun = fun() -> qlc:e(Q) end,
    mnesia_transaction(Fun).

mnesia_transaction(Fun) ->
    case mnesia:sync_transaction(Fun) of
        {aborted, Reason} ->
            ?LOG_ERROR("access regdb failed",[{error, Reason}]),
            {aborted, Reason};
        Result -> Result
    end.

connect_to_alertor_server(State=#state{client=offline, reset_ref=undefined}) ->
    do_connect_to_alertor_server(State);
connect_to_alertor_server(State=#state{client=offline, reset_ref=Ref}) ->
    erlang:cancel_timer(Ref),
    do_connect_to_alertor_server(State#state{reset_ref=undefined}).
do_connect_to_alertor_server(State=#state{client=offline, delay=Delay}) ->
    {Next, {IP,Port}} = get_server_addr(State),
    try thrift_client_util:new(IP,Port,alertor_thrift,[{framed, true}]) of
        {ok, Client} ->
            ?LOG_INFO("connect to alertor server succeed",[{IP,Port}]),
            R = erlang:send_after(?RECONN_DELAY_MAX, self(), reset_delay),
            State#state{client=Client,index=Next,reset_ref=R}
    catch
        T:E ->
            erlang:send_after(Delay, self(), reconnect),
            ?LOG_ERROR("connect to alertor server failed",[{T,E},{IP,Port}]),
            State#state{delay=backoff(Delay),index=Next}
    end.
backoff(Delay) ->
    New = Delay*2,
    if New > ?RECONN_DELAY_MAX -> ?RECONN_DELAY_MAX;
                          true -> New
    end.

get_server_addr(_State=#state{index=Index}) ->
    Addrs = get_config(alertor_addr,[{{127,0,0,1},9110},{{127,0,0,1},9111}]),
    Next = Index+1,
    Len = length(Addrs),
    if Index > Len -> {2,    lists:nth(1,    Addrs)};
              true -> {Next, lists:nth(Index,Addrs)}
    end.


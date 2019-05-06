-module(perf_stat).
-behaviour(gen_server).
-vsn("").
%% API
-export([start_link/1, dsf_request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	     terminate/2, code_change/3]).

-include("define.hrl").
-include("reg_server.hrl").

-record(state, {hostname, dsf_request}).

-define (SERVER, ?MODULE).

-define (POLL_TIME ,30000).

%%====================================================================
%% API
%%====================================================================
start_link(StartArgs) ->
    gen_server:start_link({local, perf_stat}, ?MODULE, StartArgs, []).

dsf_request(Info) ->
    gen_server:cast(?SERVER, {dsf_request, Info}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init(_StartArgs) ->
    erlang:send_after(?POLL_TIME, self(), stat_poll),

    {ok, HostName} = inet:gethostname(),
    ?LOG_INFO("perf process init, ~p", [HostName]),
    
    K = "stat@" ++ HostName,
    F = fun() -> mnesia:delete(dsconfig, K, write) end,
    {atomic, ok} = dbutil:mnesia_transaction(F),
    
    {ok, #state{hostname=HostName, dsf_request=0}}.

handle_call(Request, _From, State) ->
    ?LOG_WARNING("handle_call : unknown request", [Request]),
    {reply, ok, State}.

handle_cast({dsf_request, _Info}, State=#state{dsf_request=R}) ->
    {noreply, State#state{dsf_request=(R + 1)}};
handle_cast(Request, State) ->
    ?LOG_WARNING("handle_cast : unknown request", [Request]),
    {noreply, State}.

handle_info(stat_poll, State) ->
    erlang:send_after(?POLL_TIME, self(), stat_poll),
    do_poll_timeout(State);
handle_info(Info, State) ->
    ?LOG_WARNING("handle_info : unknown info", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_poll_timeout(State=#state{hostname=H, dsf_request=D_R}) ->

    D_C = erlang:length(supervisor:which_children(dsf_sup)),

    K = "stat@" ++ H,
    {MS, M, _} = now(),
    L_T = MS * 1000000 + M,

    {WallClock, _} = erlang:statistics(wall_clock),
    WallSecs = WallClock div 1000,

    case dao:perf_stat(K, WallSecs, L_T, D_R, D_C) of
        {atomic, _} ->
            {noreply, State#state{dsf_request=0}};
        Error ->
            ?LOG_WARNING("perf stat write db failed", [Error]),
            {noreply, State}
    end.





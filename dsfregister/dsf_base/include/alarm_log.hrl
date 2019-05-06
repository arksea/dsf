%%%-------------------------------------------------------------------
%%% $Date: 2014-02-28 18:47:43 +0800 (周五, 28 二月 2014) $
%%% $Author: xiaohaixing_298852 $
%%%-------------------------------------------------------------------
-vsn("$Revision: 61523 $").

-define (debug_on, 1).
-define(LOG_MSG(Msg),              alarm_handler:set_alarm({log_info,Msg,[],?MODULE,?LINE})).
-define(LOG_INFO(Msg, Details),    alarm_handler:set_alarm({log_info,Msg,Details,?MODULE,?LINE})).
-define(LOG_WARNING(Msg, Details), alarm_handler:set_alarm({log_warning,Msg,Details,?MODULE,?LINE})).
-define(LOG_WARNING(Label,Msg,Details), alarm_handler:set_alarm({log_warning,Label,Msg,Details,?MODULE,?LINE})).
-define(LOG_WARNING(Label,Source,Host,Msg,Details), alarm_handler:set_alarm({log_warning,Label,Source,Host,Msg,Details,?MODULE,?LINE})).
-define(LOG_ERROR(Msg, Details), alarm_handler:set_alarm({log_error,Msg,Details,?MODULE,?LINE,erlang:get_stacktrace()})).
-define(LOG_ERROR(Label,Msg,Details), alarm_handler:set_alarm({log_error,Label,Msg,Details,?MODULE,?LINE,erlang:get_stacktrace()})).
-define(LOG_ERROR(Label,Source,Host,Msg,Details), alarm_handler:set_alarm({log_error,Label,Source,Host,Msg,Details,?MODULE,?LINE,erlang:get_stacktrace()})).
-define(LOG_RESUME(Label), alarm_handler:set_alarm({log_resume,Label,?MODULE,?LINE})).
-define(LOG_RESUME(Label,Source,Host), alarm_handler:set_alarm({log_resume,Label,Source,Host,?MODULE,?LINE})).

-ifdef(debug_on).
-define(LOG_DEBUG(Msg, Details),    alarm_handler:set_alarm({log_info,Msg,Details,?MODULE,?LINE})).
-else.
-define(LOG_DEBUG(Msg, Details), ok).
-endif.



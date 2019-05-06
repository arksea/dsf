-ifndef(_alarm_types_included).
-define(_alarm_types_included, yeah).

-define(alarm_AlarmLevel_NOTIFY, 0).
-define(alarm_AlarmLevel_WARNING, 1).
-define(alarm_AlarmLevel_ERROR, 2).
-define(alarm_AlarmLevel_FATAL, 3).

%% struct alarmException

-record(alarmException, {message :: string() | binary()}).

%% struct alarmKey

-record(alarmKey, {label :: string() | binary(),
                   source :: string() | binary(),
                   host :: string() | binary()}).

%% struct alarm

-record(alarm, {key :: #alarmKey{},
                level :: integer(),
                summary :: string() | binary(),
                timestamp :: integer(),
                detail :: string() | binary()}).

%% struct detail

-record(detail, {id :: integer(),
                 alertTime :: integer(),
                 detail :: string() | binary()}).

%% struct current

-record(current, {id :: integer(),
                  key :: #alarmKey{},
                  summary :: string() | binary(),
                  primelLevel :: integer(),
                  currentLevel :: integer(),
                  count :: integer(),
                  firstTime :: integer(),
                  lastTime :: integer(),
                  resumeTime :: integer()}).

-endif.

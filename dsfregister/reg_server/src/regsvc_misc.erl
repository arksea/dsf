-module(regsvc_misc).
-export([open_dets_file/2,get_env/2,regname_to_exists_atom/1]).
-export([now_diff_sec/1,now_diff_hour/1]).

-define(APPLICATION, reg_server).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
open_dets_file(TableName,Option) ->
    FileName = get_store_file_name(TableName),
    filelib:ensure_dir(FileName),
    dets:open_file(FileName,Option).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_store_file_name(TableName) ->
    Path = get_data_path(),
    lists:flatten(io_lib:format("~s/~w.dat",[Path,TableName])).
get_data_path() ->
    code:root_dir() ++ "/data".

get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> Value;
          undefined -> Default
    end.

regname_to_exists_atom(Bin) when is_binary(Bin) ->
    try erlang:binary_to_existing_atom(Bin,latin1)
    catch
        error:badarg -> {error,badarg}
    end;
regname_to_exists_atom(Str) when is_list(Str) ->
    try erlang:list_to_existing_atom(Str)
    catch
        error:badarg -> {error,badarg}
    end;
regname_to_exists_atom(RegName) when is_atom(RegName) ->
    RegName.

now_diff_sec(Time) ->
    timer:now_diff(now(), Time)/1000000.
now_diff_hour(Time) ->
    timer:now_diff(now(), Time)/1000000/3600.


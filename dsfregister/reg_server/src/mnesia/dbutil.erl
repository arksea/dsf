%%%-------------------------------------------------------------------
%%% $Date: 2014-03-27 17:14:53 +0800 (周四, 27 三月 2014) $
%%% $Author: xulinrong_100407 $
%%%-------------------------------------------------------------------
-module(dbutil).
-vsn("$Revision: 62014 $").
-export([init/0,import_table/1,backup_db/0,restore_db/0]).
-export([read/2,match/1,match/2,write/2,delete/2,insert/2,update/2,do_query/1,get_config/1]).
-export([foldl/3,delete_object/2]).
-export([from_jsonobj/2,to_jsonobj/2,from_jsonobj/3,from_jsonobj/4]).
-export([get_counts/1,mnesia_transaction/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("define.hrl").
-include("reg_server.hrl").
-define(APPNAME, reg_server).
%%====================================================================
%% API
%%====================================================================
init() ->
    mnesia_cluster:init(),
    ensure_static_config().

read(TableName, Key) ->
    F = fun() -> mnesia:read(TableName, Key, read) end,
    mnesia_transaction(F).

match(Record) ->
     F = fun() -> mnesia:match_object(Record) end ,
     mnesia_transaction(F).

match(TableName, Record) ->
     F = fun() -> mnesia:match_object(TableName, Record, read) end ,
     mnesia_transaction(F).

write(TableName, Record) ->
    F = fun() -> mnesia:write(TableName, Record, write) end,
    mnesia_transaction(F).

update(TableName, Record) ->
    Key = erlang:element(2,Record),
    F = fun() ->
            case mnesia:read(TableName, Key) of
                [] -> mnesia:abort(record_not_exists);
                _Records -> mnesia:write(TableName, Record, write)
            end
        end,
    mnesia_transaction(F).

insert(TableName, Record) ->
    Key = erlang:element(2,Record),
    F = fun() ->
            case mnesia:read(TableName, Key) of
                [] -> mnesia:write(TableName, Record, write);
                _Records -> mnesia:abort(key_conflict)
            end
        end,
    mnesia_transaction(F).

delete_object(TableName, Record) ->
    F = fun() -> mnesia:delete_object(TableName, Record, write) end,
    mnesia_transaction(F).

delete(TableName, ID) ->
    F = fun() -> mnesia:delete(TableName, ID, write) end,
    mnesia_transaction(F).

do_query(Q) ->
    Fun = fun() -> qlc:e(Q) end,
    mnesia_transaction(Fun).

foldl(Fun, Acc0, TableName) ->
    Fun1 = fun() -> mnesia:foldl(Fun, Acc0, TableName) end,
    mnesia_transaction(Fun1).

get_counts(Table) ->
    F = fun() ->
            mnesia:table_info(Table, size)
        end,
    mnesia_transaction(F).

%%--------------------------------------------------------------------
get_config(Name) ->
    case do_query(qlc:q([X||X<-mnesia:table(dsconfig),X#dsconfig.name =:= Name])) of
        {atomic, [{_,_,Config}]} -> {ok,Config};
        Other -> Other
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%用Json对象更新记录值，但保持Key字段不变
from_jsonobj(TableName, {obj, Values}, OldRecord, Key) ->
    Fields = mnesia:table_info(TableName,attributes),
    RecordName = mnesia:table_info(TableName,record_name),
    [RecordName|OldValues] = tuple_to_list(OldRecord),
    list_to_tuple([RecordName | decode_record_fields(Values, Fields, OldValues, Key)]).
decode_record_fields(_Values, [], _OldValues, _Key) ->
    [];
decode_record_fields(Values, [Key|Fields], [OldValue|OldValues], Key) ->
    [OldValue | decode_record_fields(Values,Fields,OldValues, Key)];
decode_record_fields(Values, [Field|Fields], [OldValue|OldValues], Key) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     OldValue
     end | decode_record_fields(Values,Fields,OldValues, Key)].
%%--------------------------------------------------------------------
%用Json对象更新记录值
from_jsonobj(TableName, {obj, Values}, OldRecord) ->
    Fields = mnesia:table_info(TableName,attributes),
    RecordName = mnesia:table_info(TableName,record_name),
    [RecordName|OldValues] = tuple_to_list(OldRecord),
    list_to_tuple([RecordName | decode_record_fields(Values, Fields, OldValues)]).
decode_record_fields(_Values, [], _OldValues) ->
    [];
decode_record_fields(Values, [Field|Fields], [OldValue|OldValues]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     OldValue
     end | decode_record_fields(Values,Fields,OldValues)].
%%--------------------------------------------------------------------
%将Json对象转换成记录
from_jsonobj(TableName, {obj, Values}) ->
    Fields = mnesia:table_info(TableName,attributes),
    RecordName = mnesia:table_info(TableName,record_name),
    list_to_tuple([RecordName | decode_record_fields(Values, Fields)]).
decode_record_fields(_Values, []) ->
    [];
decode_record_fields(Values, [Field | Rest]) ->
    [case lists:keysearch(atom_to_list(Field), 1, Values) of
	 {value, {_, Value}} ->
	     Value;
	 false ->
	     '_'
     end | decode_record_fields(Values, Rest)].
%%--------------------------------------------------------------------
%将记录转换成Json对象
to_jsonobj(TableName, Record) ->
    Fields = mnesia:table_info(TableName,attributes),
    [_|Values] = tuple_to_list(Record),
    {obj, encode_record_fields(Values,Fields)}.
encode_record_fields(_Values, []) ->
    [];
encode_record_fields([], _Fields) ->
    [];
encode_record_fields([Value|Values], [Field | Fields]) ->
    [{atom_to_list(Field), Value} | encode_record_fields(Values, Fields)].
%%--------------------------------------------------------------------
ensure_static_config() ->
    {atomic,Configs} =do_query(qlc:q([X#dsconfig.config || X <- mnesia:table(dsconfig)])),
    if  length(Configs) > 0 -> donothing;
        true ->
            import_table(dsconfig),
            import_table(svcdef),
            import_table(svcdef_idl),
            import_table(dservice_state)
    end.
import_table(TableName) ->
    {ok, Vsn} = application:get_key(?APPNAME, vsn),
    Path = code:root_dir()++"/releases/"++Vsn++"/table_"++atom_to_list(TableName)++".import",
    {ok, Records} = file:consult(Path),
    io:format("import table ~p:~n",[TableName]),
    [import_one_record(TableName, R)||R<-Records],
    io:format("~n",[]).
import_one_record(TableName, R) ->
    Ret = write(TableName,R),
    io:format("~p: ~p~n",[Ret,R]).

%从备份文件中导入数据
restore_db() ->
    {ok, Vsn} = application:get_key(?APPNAME, vsn),
    Path = code:root_dir() ++ "/releases/" ++ Vsn ++ "/regdb.bak",
    Ret = mnesia:restore(Path,[]),
    io:format("restore regdb:~p~n",[Ret]).
%备份数据库
backup_db() ->
    {ok, Vsn} = application:get_key(?APPNAME, vsn),
    Path = code:root_dir() ++ "/releases/" ++ Vsn ++ "/regdb.bak",
    Ret = mnesia:backup(Path),
    io:format("backup regdb:~p~n",[Ret]).

mnesia_transaction(Fun) ->
    case mnesia:sync_transaction(Fun) of
        {aborted, Reason} ->
            ?LOG_ERROR("access regdb failed",[{aborted, Reason},Fun]),
            {aborted, Reason};
        Result -> Result
    end.

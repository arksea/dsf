%% JSON - RFC 4627 - for Erlang
%%---------------------------------------------------------------------------
%% Copyright (c) 2007-2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% Copyright (c) 2007-2010 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%%
%% Convenience macros for encoding and decoding record structures.
%%
%% Erlang's compile-time-only notion of record definitions means we
%% have to supply a constant record name in the source text.
-export_type([json/0,jsonobj/0,jsonkey/0,jsonarray/0,jsonnum/0]).

-define(JSON_FROM_RECORD(RName, R),
	jsonrfc:from_record(R, RName, record_info(fields, RName))).

-define(JSON_TO_RECORD(RName, R),
	jsonrfc:to_record(R, #RName{}, record_info(fields, RName))).

%-define(JSON_FLOAT_TO_LIST(Num), float_to_list(Num)).
-define(JSON_FLOAT_TO_LIST(Num), lists:flatten(io_lib:format("~w",[Num]))).

-type json() :: jsonobj() | jsonarray() | jsonnum() | jsonstr() | true | false | null.
-type jsonobj() :: {obj, [{jsonkey(), json()}]}.
-type jsonkey() :: string().
-type jsonstr() :: binary().
-type jsonarray() :: [json()].
-type jsonnum() :: integer() | float().





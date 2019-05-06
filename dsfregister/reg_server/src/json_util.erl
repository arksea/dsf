-module(json_util).
-export([term_to_jsonstr/1]).

term_to_jsonstr(Term) ->
    Str=lists:flatten(io_lib:format("~p",[Term])),
    list_to_binary(Str).

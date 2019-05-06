-module(web_index_service).

-export([handle_request/1]).
-include("yaws_api.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("define.hrl").

handle_request(Args) ->
    Method = method(Args),
    Path = Args#arg.pathinfo,
    handle_request_by_path(Path, Method, Args).


method(Arg) ->
  Req = Arg#arg.req,
  Req#http_request.method.

handle_request_by_path(undefined,_Method,_Args) ->
    {html,""};
handle_request_by_path(Path,Method,Args) ->
    %?LOG_DEBUG("request",[Path,Method,Args]),
    PathInfo = string:tokens(Path, "/"),
    handle_request_by_pathinfo(PathInfo, Method, Args).

handle_request_by_pathinfo([],_Method,_Args) ->
    usage("路径不存在");
handle_request_by_pathinfo([Name|PathInfo],Method,Args) ->
    case ets:lookup(view_module_map,Name) of
        [] ->
            Ctrl = "ctrl_" ++ Name,
            File = Ctrl ++ ".beam",
            case code:where_is_file(File) of
                non_existing ->
                   usage("请求的页面不存在");
                _ ->
                    Module = list_to_atom(Ctrl),
                    ets:insert(view_module_map,{Name,Module}),
                    erlang:apply(Module,handle_request,[Method,PathInfo,Args])
            end;
        [{_View,Module}] ->
            erlang:apply(Module,handle_request,[Method,PathInfo,Args])
    end.


usage(ErrorInfo) ->
    Version = "version : 1.0",
    Url = "usage : http://IP:Port/index.yaws/query/lang",
    {html,lists:flatten(io_lib:format("info ~s <br> API ~s <br><br>  ~s <br>",[ErrorInfo, Version, Url]))}.


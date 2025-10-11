%% 代码示例bank服务器的接口开放与回调实现模块
-module(my_bank).
-export([start/0, stop/0, new_account/1, deposit/2, withdraw/2]).

-define(SERVER, ?MODULE).

start() ->
	gen_server:start_link({local,?SERVER}, ?SERVER, init(),[]).

init() ->
	dict:new().

withdraw(_Arg0, _Arg1) ->
	erlang:error(not_implemented).

new_account(_Arg0) ->
	erlang:error(not_implemented).

deposit(_Arg0, _Arg1) ->
	erlang:error(not_implemented).

stop() ->
	erlang:error(not_implemented).
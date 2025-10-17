-module(tail_b_pro).
-export([start/0,monitor/0]).

-define(WORKER_NODE, 'worker@172.22.2.101').
-define(COOKIE, "caigou").


start() ->
	%% 启动新进程监听消息,并返回进程名
	register(log_monitor,spawn(fun() -> monitor() end)).

print_lines(Bin) ->
	%% 按行分割成二进制列表
	io:format("打印历史十行信息~n"),
	Lines = binary:split(Bin, <<"\n">>, [global]),
	%% 遍历打印，每行转换为字符串列表
	lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Lines).

monitor() ->
	receive
		{start,{log,Bin}} -> print_lines(Bin),
			monitor();
		{log,Line} ->
			io:format("~s", [Line]),
			monitor();
		{success, SuccessFilePath} ->
			io:format("接受到成功信号，删除成功日志文件并退出程序~n"),
			file:delete(SuccessFilePath)
	end.
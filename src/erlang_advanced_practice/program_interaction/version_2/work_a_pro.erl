%% 此处测试使用局域网内的两个ip来启动erl节点，分别在linux虚拟机和windows上
%% 虚拟机ip 172.22.2.101   windows-ip 172.22.2.84
-module(work_a_pro).
-export([run/0, get_log_file_last_lines/1]).

-define(MONITOR_NODE, 'log_monitor@172.22.2.84').
-define(LOG_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/manager.log").
-define(SUCCESS_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/success.log").

run() ->
	{ok, LogIoDevice} = file:open(?LOG_FILE_PATH, [append, binary]),
	{ok, SuccessIoDevice} = file:open(?SUCCESS_FILE_PATH, [append, binary]),
%%	MonitorProcessName = rpc:call(?MONITOR_NODE,tail_b_pro,start,[]),
	MonitorProcessName = log_monitor,
	{MonitorProcessName, ?MONITOR_NODE} ! {start, get_log_file_last_lines(10)},
	loop(0,LogIoDevice,SuccessIoDevice,MonitorProcessName,?MONITOR_NODE),
	file:close(LogIoDevice),
	file:close(SuccessIoDevice),
	ok.


loop(WorkTime, LogIoDevice, SuccessIoDevice,ProcessName, Node) ->
	erlang:send_after(2000, self(), {print, WorkTime + 2}),
	receive
		{print, NewWorkTime} ->
			%% 此条当前时间输出用于确认是否是每隔两秒执行
			io:format("now: ~p~n", [calendar:now_to_local_time(os:timestamp())]),
			case NewWorkTime >= 30 of
				true ->
					OutPutString = io_lib:format("i finish my work --> ~p~n", [NewWorkTime]),
					io:format(SuccessIoDevice, "~s",[list_to_binary(OutPutString)]),
					{ProcessName,Node} ! {log,list_to_binary(OutPutString)}, % 因为不需要监控进程回送消息给我们，所以不传自己的地址
					ok;
				false ->
					OutPutString = io_lib:format("i am working ~p~n", [NewWorkTime]),
					io:format(LogIoDevice, "~s", [list_to_binary(OutPutString)]),
					{ProcessName,Node} ! {log,list_to_binary(OutPutString)}, % 因为不需要监控进程回送消息给我们，所以不传自己的地址
					loop(NewWorkTime, LogIoDevice, SuccessIoDevice,ProcessName,Node)
			end
	end.

get_log_file_last_lines(N) ->
%%	IoDevice = file:open(FilePath,[read,binary]),
	case file:read_file(?LOG_FILE_PATH) of
		{ok, Bin} ->
			%% 因为在windows中换行可能是\r\n,而linux是\n,统一替换成\n再处理
			NormalBin = binary:replace(Bin, [<<"\r\n">>], <<"\n">>, [global]),
%%			StrList = binary_to_list(Bin)
			Lines = binary:split(NormalBin, <<"\n">>, [global]),  % 返回的行数据列表
			Total = length(Lines),
			io:format("总行数 ~p~n",[Total]),
			io:format(Bin),
			io:format("总行数 ~p~n",[Total]),

			OutPutLines =
				case Total - N > 0 of
					true ->
						lists:sublist(Lines, Total - N + 1, N);
					false -> Lines
				end,
			OutPutBin = iolist_to_binary(
				lists:map(fun(Line) -> [Line, <<"\n">>] end, OutPutLines)
			),
			{log, OutPutBin};
		{error, Reason} -> {error, Reason}
	end.
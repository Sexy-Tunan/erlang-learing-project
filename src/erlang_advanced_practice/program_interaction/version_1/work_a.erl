-module(work_a).
-export([run/0]).

-define(LOG_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/manager.log").
-define(SUCCESS_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/success.log").
run() ->
	{ok, LogIoDevice} = file:open(?LOG_FILE_PATH, [append, {encoding,utf8}]),
	{ok, SuccessIoDevice} = file:open(?SUCCESS_FILE_PATH, [append, {encoding,utf8}]),
	loop(0,LogIoDevice,SuccessIoDevice),
	file:close(LogIoDevice),
	file:close(SuccessIoDevice),
	ok.

loop(WorkTime, LogIoDevice, SuccessIoDevice) ->
	erlang:send_after(2000, self(), {print, WorkTime + 2}),
	receive
		{print, NewWorkTime} ->
			%% 此条当前时间输出用于确认是否是每隔两秒执行
			io:format("now: ~p~n", [calendar:now_to_local_time(os:timestamp())]),
			case NewWorkTime >= 30 of
				true -> io:format(SuccessIoDevice, "success --> ~p~n", [NewWorkTime]), ok;
				false -> io:format(LogIoDevice, "i am working ~p~n", [NewWorkTime]),
					loop(NewWorkTime, LogIoDevice, SuccessIoDevice)
			end
	end.


%% 出现的错误
%% ** exception error: no_translation
%% device failed to transcode string from unicode to latin1
%% 原因---》 io:format/3 默认假定目标设备（LogIoDevice）使用 latin1 字符编码，字符串 "我正在工作" 是 UTF-8 中文。
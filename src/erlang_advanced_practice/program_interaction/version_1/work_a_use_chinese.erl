-module(work_a_use_chinese).
-export([run/0]).

-define(LOG_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/manager.log").
-define(SUCCESS_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/success.log").
run() ->
	{ok, LogIoDevice} = file:open(?LOG_FILE_PATH, [append, binary]),
	{ok, SuccessIoDevice} = file:open(?SUCCESS_FILE_PATH, [append, binary]),
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
				true -> io:format(SuccessIoDevice, "~s", [unicode:characters_to_binary(io_lib:format("成功 --> ~p~n", [NewWorkTime]),utf8,utf8)]), ok;
				false -> io:format(LogIoDevice, "~s", [unicode:characters_to_binary(io_lib:format("我正在工作 ~p~n", [NewWorkTime]),utf8,utf8)]),
					loop(NewWorkTime, LogIoDevice, SuccessIoDevice)

			end
	end.
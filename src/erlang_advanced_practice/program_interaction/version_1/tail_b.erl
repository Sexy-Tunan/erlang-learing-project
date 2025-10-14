%% 解决思路：第一次读取整个文件内容，输出尾十行
%% 接着死循环不断读取文件信息，如果大小发生变化，则通过移动指针来输出

-module(tail_b).
-export([run/0]).

-define(BE_MONITORED_FILE_PATH, "src/erlang_advanced_practice/program_interaction/version_1/manager.log").
-include_lib("kernel/include/file.hrl").

run() ->
	monitor_file_change(?BE_MONITORED_FILE_PATH,print_last_lines(?BE_MONITORED_FILE_PATH,10)),
	ok.


print_last_lines(FilePath,N) ->
	case file:read_file(FilePath) of
		{ok, Bin} ->
			%% 因为在windows中换行可能是\r\n,而linux是\n,统一替换成\n再处理
			NormalBin = binary:replace(Bin,<<"\r\n">>,<<"\n">>, [global]),
			Lines = binary:split(NormalBin,<<"\n">>, [global]),  % 返回的行数据列表
			Total = length(Lines),
			case Total-N > 0 of
				true ->
					OutPutLines = lists:sublist(Lines,Total - N + 1,N),
					lists:foreach(fun(Line) -> io:format("~s~n",[Line])end,OutPutLines);
				false -> lists:foreach(fun(Line) -> io:format("~s~n",[Line])end,Lines)
			end,
			% 此处返回文件大小
			byte_size(Bin);
		{error,Reason} -> {error,Reason}
	end.


monitor_file_change(FilePath, LastSizeRead) ->
	% 每隔一秒循环一次即可
	timer:sleep(1000),
	case file:read_file_info(FilePath) of
		{ok, FileInfo} ->
			NewSize = FileInfo#file_info.size,
			case NewSize > LastSizeRead of
				true ->
					NeedReadSize = NewSize - LastSizeRead,
					{ok, FileIoDevice} = file:open(FilePath, [read, binary]),
					{ok, _} = file:position(FileIoDevice, LastSizeRead),
					case file:read(FileIoDevice, NeedReadSize) of
						{ok, AppendNewBin} -> io:format("~s", [AppendNewBin])
					end,
					file:close(FileIoDevice),
					monitor_file_change(FilePath, NewSize);
				false -> monitor_file_change(FilePath, LastSizeRead)
			end
	end.


%% 返回的FileInfo是一个记录 -》 https://www.erlang.org/docs/28/apps/kernel/file.html#t:file_info/0
%% 需引入对应的记录声明，否则报错解析不了file_info记录

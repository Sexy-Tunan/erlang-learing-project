-module(checker).
-export([check/1]).

%  编译Erlang文件X.erl后会生成一个X.beam文件（如果编译成功的话）。编写一个程序来
%  检查某个Erlang模块是否需要重新编译。做法是比较相关Erlang文件和beam文件的最后修改时间戳。
% 
%% 检查模块
check(ModuleName) -> 
    
    % 分别获取.erl与.beam文件的修改时间
    ErlFileTime = getFileModifyTime(ModuleName, ".erl"),
    BeamFileTime = getFileModifyTime(ModuleName, ".beam"),
 
    case ErlFileTime of
        false -> exit("Erl File Not Found");
        _ -> true 
    end,
 
    case BeamFileTime of
        false -> exit("Beam File Not Found");
        _ -> true 
    end,
 
    % 比较两者的时间
    case ErlFileTime =:= BeamFileTime of
        true -> io:format("IT'S NEW~n");
        false -> io:format("NEED TO COMPILE AGAIN~n")
    end.
 
%% 获取文件的修改时间
 
getFileModifyTime(ModuleName, Prefix) ->
    try
        Status = file:read_file_info(ModuleName ++ Prefix), 
        {ok,{file_info,
                _Size, _Type, _Access,_Atime,Mtime,_CTime,
                _Mode,_Links,_MajorDevice,_MinorDevice,_Inode,_Uid,_Gid}} = Status,
        
        {{Year, Month, Day}, {Hour, Minute, _Second}} = Mtime,
                
        {Year, Month, Day, Hour, Minute}             % 返回文件的修改时间，精确到分
     
    catch
        error:_X -> false
    end.
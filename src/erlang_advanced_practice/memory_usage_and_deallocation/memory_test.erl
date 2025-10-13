%% process
%% (1)插入 100万记录。查看占用了多少erlang进程、系统进程的内存。（用BIF、Linux TOP命令等）
%% (2)数据全删除后。还占用了多少erlang进程、系统进程的内存。
%% (3)执行erlang:garbage_collect()垃圾回收后，查看erlang进程、系统进程的内存。
%%
%% ets
%% (1)插入 100万记录。查看占用了多少erlang进程、系统进程的内存。（用BIF、Linux TOP命令等）
%% (2)数据全删除后。还占用了多少erlang进程、系统进程的内存。
%% (3)执行erlang:garbage_collect()垃圾回收后，查看erlang进程、系统进程的内存。
%%
%% Process state
%% (1)插入 100万记录。查看占用了多少erlang进程、系统进程的内存。（用BIF、Linux TOP命令等）
%% (2)数据全删除后。还占用了多少erlang进程、系统进程的内存。
%% (3)执行erlang:garbage_collect()垃圾回收后，查看erlang进程、系统进程的内存。
-module(memory_test).
-export([run/0]).

run() -> 
    test_process(),
    test_ets(),
    test_process_state(),
    ok.

test_process() ->
    % 往进程字典中插入一百万条记录
    io:format("=======================================================~n"),
    io:format("测试进程字典的对erlang和系统进程的占用~n"),
    io:format("往进程字典中插入 100 万记录中...~n"),
    OneMillionElementList = lists:seq(1, 1000000),
    lists:foreach(fun(I) -> put(I,I*2) end, OneMillionElementList),
    io:format("插入完毕！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    % 删除数据
    io:format("删除全部数据！~n"),
    erase(),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    % 垃圾回收器回收
    io:format("手动回收垃圾~n"),
    erlang:garbage_collect(),
    check_memory(),
    io:format("=======================================================~n"),
    ok.

test_ets() ->
    % 往ets内存字典中插入一百条万记录
    io:format("=======================================================~n"),
    io:format("测试ets的对erlang和系统进程的占用~n"),
    PrivateOrderSet = ets:new(temp_ets,[ordered_set, private]),
    OneMillionElementList = lists:seq(1, 1000000),
    io:format("往ets中插入 100 万记录中...~n"),
    lists:foreach(fun(I) -> ets:insert(PrivateOrderSet, {I,I*2}) end, OneMillionElementList),
    io:format("插入完毕！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    % 删除数据
    io:format("删除全部数据！~n"),
    ets:delete_all_objects(PrivateOrderSet),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    % 垃圾回收器回收
    io:format("执行垃圾回收~n"),
    erlang:garbage_collect(),
    check_memory(),
    io:format("=======================================================~n"),
    ok.

test_process_state() ->
    % 使用map来存储状态
    % 往map中存储一百万个元素
    io:format("=======================================================~n"),
    io:format("测试进程状态对erlang和系统进程的占用~n"),
    io:format("往maps中插入 100 条万记录中...~n"),
    MapState = maps:new(),
    NewMapState = store_in_maps(1,MapState),
    io:format("插入完毕！~n"),
    check_memory(),

    % 删除数据
    io:format("删除全部数据！~n"),
    clear_maps(1,NewMapState),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    % 垃圾回收器回收
    io:format("执行垃圾回收~n"),
    erlang:garbage_collect(),
    check_memory(),io:format("=======================================================~n"),

    ok.
store_in_maps(Count, MapState) ->
    case Count > 1000000 of
        true -> MapState;
        false -> NewMapState = maps:put(Count,Count*2, MapState),
            store_in_maps(Count+1,NewMapState)
    end.
clear_maps(Count,MapState) ->
    case Count > 1000000 of
        true -> MapState;
        false -> NewMapState = maps:remove(Count, MapState),
            clear_maps(Count+1,NewMapState)
    end.


check_memory() ->
    check_erlang_process_memory(),
    check_sys_process_memory().

check_erlang_process_memory() ->
    io:format("erlang 进程内存信息~p 单位:Byte~n",[process_info(self(),memory)]).
check_sys_process_memory() ->
    %% 用 ps 命令获取 RSS(kB) 和 VSZ(kB)
    Pid = os:getpid(),
    Cmd = io_lib:format("ps -o rss=,vsz= -p ~s", [Pid]),
    Output = os:cmd(lists:flatten(Cmd)), % 通过os模块调用shell的ps命令
%%    io:format("ps output ~p~n",[Output]), % 我想看一下输出到底是啥，输出类似是"ps output "160152 1913356\n""
    %% 去掉多余空格换行并分割
    Clean = string:trim(Output),
    [RSSStr, VSZStr] = string:tokens(Clean, " "),
    {RSS, _} = string:to_integer(string:trim(RSSStr)),
    {VSZ, _} = string:to_integer(string:trim(VSZStr)),

    io:format("系统进程内存信息:~n"),
    io:format("  PID: ~s~n", [Pid]),
    io:format("  RSS(物理内存): ~p kB (~p B || ~.2f MB)~n", [RSS, RSS*1024 ,RSS / 1024]),
    io:format("  VSZ(虚拟内存): ~p kB (~p B || ~.2f MB)~n~n", [VSZ, VSZ*1024 ,VSZ / 1024]).

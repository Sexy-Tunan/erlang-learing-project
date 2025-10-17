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
-export([run/0, test_process_state/0, test_process/0, test_ets/0]).

run() -> 
    test_process(),
    test_ets(),
    test_process_state(),
    ok.

test_process() ->
    io:format("=======================================================~n"),
    io:format("测试进程字典的对erlang和系统进程的占用~n"),
    io:format("内存初始状态~n"),
    check_memory(),
    % 往进程字典中插入一百万条记录
    io:format("-------------------------------------------------------~n"),
    io:format("往进程字典中插入 100 万记录中...~n"),
    OneMillionElementList = lists:seq(1, 1000000),
    lists:foreach(fun(I) -> put(I,I*2) end, OneMillionElementList),
    io:format("插入完毕！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    io:format("-------------------------------------------------------~n"),

    % 删除数据
    io:format("删除全部数据！~n"),
    erase(),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    timer:sleep(10000),
    check_memory(),

    io:format("-------------------------------------------------------~n"),

    % 垃圾回收器回收
    io:format("手动回收垃圾~n"),
    erlang:garbage_collect(),
    timer:sleep(10000),
    check_memory(),
    io:format("=======================================================~n"),
    ok.

test_ets() ->
    io:format("=======================================================~n"),
    io:format("测试ets的对erlang和系统进程的占用~n"),
    io:format("内存初始状态~n"),
    check_memory(),
    % 往ets内存字典中插入一百条万记录
    io:format("-------------------------------------------------------~n"),
    PrivateOrderSet = ets:new(temp_ets,[ordered_set, private]),
    OneMillionElementList = lists:seq(1, 1000000),
    io:format("往ets中插入 100 万记录中...~n"),
    lists:foreach(fun(I) -> ets:insert(PrivateOrderSet, {I,I*2}) end, OneMillionElementList),
    io:format("插入完毕！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    check_memory(),

    io:format("-------------------------------------------------------~n"),

    % 删除数据
    io:format("删除全部数据！~n"),
    ets:delete_all_objects(PrivateOrderSet),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    timer:sleep(10000),
    check_memory(),

    io:format("-------------------------------------------------------~n"),

    % 垃圾回收器回收
    io:format("执行垃圾回收~n"),
    erlang:garbage_collect(),
    timer:sleep(10000),
    check_memory(),
    io:format("=======================================================~n"),
    ok.

test_process_state() ->
    io:format("=======================================================~n"),
    io:format("测试进程状态对erlang和系统进程的占用~n"),
    io:format("内存初始状态~n"),
    check_memory(),
    % 使用map来存储状态
    % 往map中存储一百万个元素
    io:format("-------------------------------------------------------~n"),

    io:format("往maps中插入 100 条万记录中...~n"),
    MapState = maps:new(),
    NewMapState = store_in_maps(1,MapState),
    io:format("插入完毕！~n"),
    check_memory(),
    io:format("-------------------------------------------------------~n"),
    % 删除数据
    io:format("删除全部数据！~n"),
    clear_maps(1,NewMapState),
    io:format("删除完成！~n"),
    % 查看占用了多少erlang进程、系统进程的内存
    timer:sleep(10000),
    check_memory(),
    io:format("-------------------------------------------------------~n"),

    % 垃圾回收器回收
    io:format("执行垃圾回收~n"),
    erlang:garbage_collect(),
    %% 垃圾回收没这么快，erlang:garbage_collect()只是通知了，并不会同步等待垃圾回收完成，此处需休眠几秒等待垃圾回收完成再
    timer:sleep(10000),
    check_memory(),
    io:format("=======================================================~n"),
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
    Mem = erlang:memory(),
%%    Total  = erlang:memory(total),
%%    ProcessSize = erlang:memory(processes),
%%    System = erlang:memory(system),
    Total  =  proplists:get_value(total,Mem),
    Processes = proplists:get_value(processes,Mem),
    Ets =  proplists:get_value(ets,Mem),

    io:format("total-------》erlang总内存：~.2f MB~n",[Total/(1024*1024)]),
    io:format("processes---》erlang所有进程占用的内存：~.2f MB~n",[Processes/(1024*1024)]),
    io:format("ets---------》为所有ets表分配的总内存：~.2f MB~n",[Ets/(1024*1024)]).

check_sys_process_memory() ->
    %% 用 ps 命令获取 RSS(kB) 和 VSZ(kB)
    Pid = os:getpid(),
    Cmd = io_lib:format("ps -o rss= -p ~s", [Pid]),
    Output = os:cmd(lists:flatten(Cmd)), % 通过os模块调用shell的ps命令
%%    io:format("~p~n",[Output]),
    %% 去除尾部的换行符,并转换为整数
%%    io:format("~p~n",[Clean]),
    {RSS,_} = string:to_integer(string:trim(Output)), %% 我也不知道为啥"123456" 调用方法变成数字会是这种形式 -》{123456,[]}
    io:format("系统进程内存信息:~n"),
    io:format("  RSS(物理内存): ~p kB (~p B || ~.2f MB)~n", [RSS, RSS*1024 ,RSS / 1024]).

-module(question13_1).
-export([my_spawn/3, my_spawn2/3, my_spawn3/3]).

% 编写一个my_spawn(Mod, Func, Args)函数。它的行为类似spawn(Mod, Func, Args)，
% 但有一点区别。如果分裂出的进程挂了，就应打印一个消息，说明进程挂掉的原因以及在此之前
% 存活了多长时间。 
% 解决方法1：同步阻塞等待
my_spawn(Mod, Func, Args) ->
    Start = erlang:monotonic_time(millisecond),
    Pid = spawn(Mod, Func, Args),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            End = erlang:monotonic_time(millisecond),
            AliveTime = End - Start,
            io:format("Process ~p died after ~p ms. Reason: ~p~n",
                      [Pid, AliveTime, Reason])
    end.

% 解决方法2：异步返回，新创建监控进程监控
my_spawn2(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    spawn(fun() -> monitor_and_log(Pid) end),
    Pid.

monitor_and_log(Pid) ->
    Start = erlang:monotonic_time(millisecond),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            End = erlang:monotonic_time(millisecond),
            AliveTime = End - Start,
            io:format("Process ~p died after ~p ms. Reason: ~p~n",
                      [Pid, AliveTime, Reason])
    end.


% 解决方法3: 方法2优化多子进程监控
my_spawn3(Mod, Func, Args) ->
    Parent = self(),
    %% 创建一个子进程，专门来监控目标进程
    spawn(fun() ->
        StartTime = erlang:monotonic_time(millisecond),
        Pid = spawn(Mod, Func, Args),
        Ref = erlang:monitor(process, Pid),
        %% 通知调用者新进程的Pid
        Parent ! {self(), Pid},
        receive
            {'DOWN', Ref, process, Pid, Reason} ->
                EndTime = erlang:monotonic_time(millisecond),
                LifeTime = EndTime - StartTime,
                io:format("Process ~p exited with reason ~p after ~p ms~n",
                          [Pid, Reason, LifeTime])
        end
    end),
    %% 等待监控进程把目标 Pid 回传回来
    receive
        {_, Pid} -> Pid
    end.

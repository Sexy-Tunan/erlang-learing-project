-module(memory_test).
-export([run/0]).

run() ->
    Tab = ets:new(test, [set, public]),
    io:format("插入 100 万记录中...~n"),
    lists:foreach(fun(I) -> ets:insert(Tab, {I, I * 2}) end, lists:seq(1, 1000000)),
    io:format("插入完毕！~n"),
    report(Tab),
    ets:delete_all_objects(Tab),
    io:format("删除完毕！~n"),
    report(Tab),
    erlang:garbage_collect(),
    io:format("执行 erlang:garbage_collect() 后：~n"),
    report(Tab),
    ok.

report(Tab) ->
    % 1. 查看Erlang内部ETS内存使用
    {memory, EtsMem} = process_info(self(), memory),
    EtsInfo = ets:info(Tab, memory),
    io:format("当前进程内存: ~p bytes~nETS表内存: ~p bytes~n", [EtsMem, EtsInfo]),

    % 2. 系统层面（Linux top）
    io:format("请使用 Linux 'top' 命令查看系统整体内存变化：~n  top -p <beam.smp pid>~n~n"),
    ok.

%% cpu_demo.erl
%% 利用质数计算cpu密集型
%% 用 spawn 多进程做 CPU 密集型计算，并测时。
-module(cpu_demo).
-export([test/0,pmap/2,fib/1,equal/2]).

test() ->
%% L = [27,27,27,..] 共100个
	L = lists:duplicate(100, 27),
	{Time1, S1} = timer:tc(lists, map, [fun cpu_demo:fib/1, L]),
	{Time2, S2} = timer:tc(cpu_demo, pmap, [fun cpu_demo:fib/1, L]),
	{fib, Time1, Time2, equal(S1, S2)}.


%% 递归（低效率）的斐波那契
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).


%% 课本pmap代码示例
pmap(F, L) ->
	S = self(),
	%% make_ref() returns a unique reference
	%%   we'll match on this later
	Ref = erlang:make_ref(),
	Pids = lists:map(fun(I) ->
		spawn(fun() -> do_f(S, Ref, F, I) end)
			   end, L),
	%% gather the results
	gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
	Parent ! {self(), Ref, (catch F(I))}.
gather([Pid|T], Ref) ->
	receive
		{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
	end;
gather([], _) ->
	[].

equal(S,S)   -> true;
equal(S1,S2) ->  {differ, S1, S2}.
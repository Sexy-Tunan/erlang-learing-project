%% cpu_demo.erl
%% 利用质数计算cpu密集型
%% 用 spawn 多进程做 CPU 密集型计算，并测时。
-module(cpu_demo2).
-export([test/0,pmap/2,is_prime/1,equal/2]).

test() ->
	% 重置随机数种子
	_ = rand:seed(exsplus, now_to_seed()),
	% 生成500个列表，每个列表包含50000个介于300000~500000数值之间的数字
	S = lists:seq(1,16),
	Lists = [make_inner_list(50000) || _ <- S],
	{Time1, S1} = timer:tc(fun() ->
		lists:map(fun(List) ->
			lists:map(fun is_prime/1, List) end, Lists) end),

	{Time2,S2} = timer:tc(fun() ->
		pmap(fun(List) -> lists:map(fun is_prime/1, List) end, Lists) end),

	{prime_judge,Time1, Time2, equal(S1,S2)}.


% 生成一个包含 N 个随机整数的列表（300000 ~ 500000）
make_inner_list(N) ->
	[300000 + rand:uniform(200001) - 1 || _ <- lists:seq(1, N)].

%% 判断一个数是否为质数
%% 极低效版本：从 2 遍历到 N-1
is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) ->
	Max = trunc(math:sqrt(N)),
	is_prime(N, 2, Max).

is_prime(N, I, Max) when I > Max -> true;
is_prime(N, I, Max) ->
	case N rem I of
		0 -> false;
		_ -> is_prime(N, I + 1, Max)
	end.


now_to_seed() ->
	{A,B,C} = now(),
	{A rem 30000, B rem 30000, C rem 30000}.


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
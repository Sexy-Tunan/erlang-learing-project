%% cpu_demo.erl
%% 利用质数计算测试多核与单核性能对比
-module(cpu_demo3).
-export([test/0,pmap/2,is_prime/1,equal/2]).

test() ->
	% 生成500个列表，每个列表包含10000个介于300000~500000数值之间的数字
	List = make_inner_list(16),
	{Time1, S1} = timer:tc(fun() -> lists:map(fun count_primes/1, List) end),

	{Time2,S2} = timer:tc(fun() ->
		pmap(fun count_primes/1, List) end),

	{prime_judge,Time1, Time2, equal(S1,S2)}.


% 生成一个包含 一个包含范围的元组列表
make_inner_list(N) ->
	[{X*100000,(X+1)*100000} || X <- lists:seq(1, N)].


%% 判断一个数是否为质数
%% 极低效版本：从 2 遍历到 N-1
is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) ->
	Max = trunc(math:sqrt(N)),
	not lists:any(fun(I) -> N rem I == 0 end, lists:seq(2, Max)).
%% 计算Low -> High有多少个指数
count_primes({Low, High}) ->
	count_primes(Low,High).

count_primes(Low, High) ->
	length([N || N <- lists:seq(Low, High), is_prime(N)]).


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
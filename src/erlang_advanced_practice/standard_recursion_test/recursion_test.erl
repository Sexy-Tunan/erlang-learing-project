%% 测试一下这两个函数(suma,sumb)的调用，如果列表参数是1~1百万的时候，分别耗时多少。
%%
%% %%不规范的递归
%% suma([])-> 0;
%% suma([H|T])-> H+suma(T).
%%
%% %%使用尾递归
%% sumb([],Acc)-> Acc;
%% sumb([H|T],Acc)-> sumb(T,Acc+H).

-module(recursion_test).
-export([run/1, suma/1, sumb/2]).

run(Length) ->
	case Length >= 1 andalso Length =< 1000000 of
		true -> OneMillionElementList = lists:seq(1,Length),
			{Time1, _} = timer:tc(?MODULE, suma, [OneMillionElementList]),
			io:format("suma - add 1 to ~p,  cost_time= ~p 微秒~n",[Length,Time1]),
			{Time2, _} = timer:tc(?MODULE, sumb, [OneMillionElementList,0]),
			io:format("sumb - add 1 to ~p,  cost_time= ~p 微秒~n",[Length,Time2]);
		false -> {error, "Length只能为1到1000000"}
	end.


% 不规范
suma([]) -> 0;
suma([H|T]) -> H + suma(T).

% 规范,尾递归
sumb([],Acc) -> Acc;
sumb([H|T],Acc) -> sumb(T,Acc+H).
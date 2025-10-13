%% 8.测试一下两个不同大小的列表的拼接的性能情况。
%% 列表A只有一个元素，列表B有一万个元素。
%% 列表A++B 的耗时
%% 列表B++A 的耗时

-module(splice_list_test).
-export([run/0, huge_splice_small/2, small_splice_huge/2]).

run() ->
	HugeList = lists:seq(1,10000),
	SmallList = [1],

	{Time1, _} = timer:tc(?MODULE, small_splice_huge, [SmallList,HugeList]),
	{Time2, _} = timer:tc(?MODULE, huge_splice_small, [HugeList,SmallList]),
	io:format("列表A只有一个元素，列表B有一万个元素~n"),
	io:format("A ++ B 的时间为 ~p 微秒 ~n",[Time1]),
	io:format("B ++ A 的时间为 ~p 微秒 ~n",[Time2]),
	ok.

huge_splice_small(HugeList, SmallList) ->
	HugeList ++ SmallList.

small_splice_huge(HugeList, SmallList) ->
	SmallList ++ HugeList.
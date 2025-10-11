-module(adapter_db).
-export([test/0, make/1, store/3, lookup/2]).

test() ->
	%% 测试dict模块
	M0 = make(dict),
	M1 = M0:store(key1, val1),
	M2 = M1:store(key2, val2),
	{ok, val1} = M2:lookup(key1),
	{ok, val2} = M2:lookup(key2),
	error = M2:lookup(nokey),
	%% 测试list模块
	N0 = make(lists),
	N1 = N0:store(key1, val1),
	N2 = N1:store(key2, val2),
	{ok, val1} = N2:lookup(key1),
	{ok, val2} = N2:lookup(key2),
	error = N2:lookup(nokey),
	ok.

make(dict) ->
	{?MODULE, dict, dict:new()};
make(lists) ->
	{?MODULE, lists, []}.

store(Key, Val, {?MODULE, dict, D}) ->
	lists:sum([1,2]),
	D1 = dict:store(Key, Val, D),
	{?MODULE, dict, D1};
store(Key, Val, {?MODULE, lists, L}) ->
	{?MODULE, lists, lists:keystore(Key, 1, L, {Key, Val})}.

lookup(Key, {?MODULE, dict, D}) ->
	dict:find(Key, D);
lookup(Key, {?MODULE, lists, L}) ->
	case lists:keysearch(Key, 1, L) of
		{value, {Key, Val}} ->
			{ok, Val};
		false ->
			error
	end.
%%  程序做的第一件事是用一个非常大的单词集合来计算英语中所有的三字母组合。要做到这一
%%  点，我们使用ETS异键表。这个决定是根据ETS异键表、有序异键表和sets模块所提供的“纯”
%%   Erlang异键表之间的一组相对性能测量结果得出的。 
%%  我们将在接下来的几节里做这些事。 
%%  (1) 制作一个遍历英语里所有三字母组合的迭代函数。这将大大简化向不同表类型插入三字
%%  母组合的编码工作。 
%%  (2) 创建set和ordered_set类型的ETS表来存放所有这些三字母组合。另外再创建一个异键
%%  表来存放这些三字母组合。 
%%  (3) 测量创建这几种表所需的时间。 
%%  (4) 测量访问这几种表所需的时间。 
%%  (5) 根据测量结果选择最佳的方法，并为它编写访问函数。
%% 
%% 
%% 定义一个名为for_each_trigram_in_the_english_language(F, A)的函数。这个函数会
%% 把fun F应用到英语里的每一个三字母组合上。F是一个类型为fun(Str, A) -> A的fun，Str涵
%% 盖了英语里所有的三字母组合，A则是一个累加器
-module(lib_trigrams).
-export([for_each_trigram_in_the_english_language/2,
	 make_tables/0, timer_tests/0,
	 open/0, close/1, is_word/2,
	 how_many_trigrams/0, 
	 make_ets_set/0, make_ets_ordered_set/0, make_mod_set/0,
	 lookup_all_ets/2, lookup_all_set/2
	]).
-import(lists, [reverse/1]).

%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------
%% 测试各种表插入所有三元组消耗的时间，以及平均每个的插入用时
make_tables() ->
    {Micro1, N} = timer:tc(?MODULE, how_many_trigrams, []),
    io:format("Counting - No of trigrams=~p time/trigram=~p~n",[N,Micro1/N]),
    {Micro2, Ntri} = timer:tc(?MODULE, make_ets_ordered_set, []),
    FileSize1 = filelib:file_size("trigramsOS.tab"),
    io:format("Ets ordered Set size=~p time/trigram=~p~n",[FileSize1/Ntri, 
			 			           Micro2/N]),
    {Micro3, _} = timer:tc(?MODULE, make_ets_set, []),
    FileSize2 = filelib:file_size("trigramsS.tab"),
    io:format("Ets set size=~p time/trigram=~p~n",[FileSize2/Ntri, Micro3/N]),
    {Micro4, _} = timer:tc(?MODULE, make_mod_set, []),
    FileSize3 = filelib:file_size("trigrams.set"),
    io:format("Module sets size=~p time/trigram=~p~n",[FileSize3/Ntri, Micro4/N]).

make_ets_ordered_set() -> make_a_set(ordered_set, "trigramsOS.tab").
make_ets_set()         -> make_a_set(set, "trigramsS.tab").

make_a_set(Type, FileName) ->		
    Tab = ets:new(table, [Type]),
    % 此处的F匿名函数并不像统计单词字典中有多少三元组那样统计数量，反而变成了不管数量，而是将每个三元组尝试插入到表中
    F = fun(Str, _) -> ets:insert(Tab, {list_to_binary(Str)}) end,
    for_each_trigram_in_the_english_language(F, 0),
    ets:tab2file(Tab, FileName),
    Size = ets:info(Tab, size),
    ets:delete(Tab),
    Size.

make_mod_set() ->
    D = sets:new(),
    F = fun(Str, Set) -> sets:add_element(list_to_binary(Str),Set) end,
    D1 = for_each_trigram_in_the_english_language(F, D),
    file:write_file("trigrams.set", [term_to_binary(D1)]).
%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------


%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------
%% 测试查询每种表查询三元组的测试用时
timer_tests() ->
    time_lookup_ets_set("Ets ordered Set", "trigramsOS.tab"),
    time_lookup_ets_set("Ets set", "trigramsS.tab"),
    time_lookup_module_sets().

time_lookup_ets_set(Type, File) ->
    {ok, Tab} = ets:file2tab(File),
    L = ets:tab2list(Tab),
    Size = length(L),
    {M, _} = timer:tc(?MODULE, lookup_all_ets, [Tab, L]),
    io:format("~s lookup=~p micro seconds~n",[Type, M/Size]),
    ets:delete(Tab).

lookup_all_ets(Tab, L) ->
    lists:foreach(fun({K}) -> ets:lookup(Tab, K) end, L).

time_lookup_module_sets() ->
    {ok, Bin} = file:read_file("trigrams.set"),
    Set = binary_to_term(Bin),
    Keys = sets:to_list(Set),
    Size = length(Keys),
    {M, _} = timer:tc(?MODULE, lookup_all_set, [Set, Keys]),
    io:format("Module set lookup=~p micro seconds~n",[M/Size]).

lookup_all_set(Set, L) ->
    lists:foreach(fun(Key) -> sets:is_element(Key, Set) end, L).
%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------


%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------
%% 从英语单词表中能构建多少个三元组
how_many_trigrams() ->
    F = fun(_, N) -> 1 + N  end,
    for_each_trigram_in_the_english_language(F, 0).
    
%% An iterator that iterates through all trigrams in the language
for_each_trigram_in_the_english_language(F, A0) ->
    {ok, Bin0} = file:read_file("/depenedncy/354984si.ngl.gz"),
    Bin = zlib:gunzip(Bin0),
    scan_word_list(binary_to_list(Bin), F, A0).

scan_word_list([], _, A) ->
    A;
scan_word_list(L, F, A) ->
    {Word, L1} = get_next_word(L, []),
    A1 = scan_trigrams([$\s|Word], F, A),
    scan_word_list(L1, F, A1).

%% scan the word looking for \r\n
%% the second argument is the word (reversed) so it
%% has to be reversed when we find \r\n or run out of characters
%% \r\n换行符表示此行单词结束
get_next_word([$\r,$\n|T], L) -> {reverse([$\s|L]), T};
get_next_word([H|T], L)       -> get_next_word(T, [H|L]);
get_next_word([], L)          -> {reverse([$\s|L]), []}.

scan_trigrams([X,Y,Z], F, A) ->
    F([X,Y,Z], A);
scan_trigrams([X,Y,Z|T], F, A) ->
    A1 = F([X,Y,Z], A),
    scan_trigrams([Y,Z|T], F, A1);
scan_trigrams(_, _, A) ->
    A.
    
%% -------------------------------------------------------------------------------------
%% -------------------------------------------------------------------------------------
%% access routines
%%   open() -> Table
%%   close(Table)
%%   is_word(Table, String) -> Bool

is_word(Tab, Str) -> is_word1(Tab, "\s" ++ Str ++ "\s").
is_word1(Tab, [_,_,_]=X) -> is_this_a_trigram(Tab, X);
is_word1(Tab, [A,B,C|D]) ->
    case is_this_a_trigram(Tab, [A,B,C]) of
	true  -> is_word1(Tab, [B,C|D]);
	false -> false
    end;
is_word1(_, _) ->
    false.
is_this_a_trigram(Tab, X) ->
    case ets:lookup(Tab, list_to_binary(X)) of
	[] -> false;
	_  -> true
    end.
open() ->
    File = filename:join(filename:dirname(code:which(?MODULE)),
			 "/trigramsS.tab"),
    {ok, Tab} = ets:file2tab(File),
    Tab.
close(Tab) -> ets:delete(Tab).
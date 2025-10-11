-module(broadcast).
-compile(export_all).

%% windows中不支持inet:ifget/2函数，必须手动指定广播地址
% send(IoList) ->
%     case inet:ifget("eth0", [broadaddr]) of
% 	{ok, [{broadaddr, Ip}]} ->
% 	    {ok, S} =  gen_udp:open(5010, [{broadcast, true}]),
% 	    gen_udp:send(S, Ip, 6000, IoList),
% 	    gen_udp:close(S);
% 	_ ->
% 	    io:format("Bad interface name, or\n"
% 		      "broadcasting not supported\n")
%     end.

send(IoList) ->
    {ok, S} = gen_udp:open(5010, [{broadcast, true}]),
    %% 手动指定广播地址
    Ip = {192,168,126,255},
    gen_udp:send(S, Ip, 6000, IoList),
    gen_udp:close(S).

listen() ->
    {ok, _} = gen_udp:open(6000),
    loop().
loop() ->
    receive
	Any ->
	    io:format("received:~p~n", [Any]),
	    loop()
    end.
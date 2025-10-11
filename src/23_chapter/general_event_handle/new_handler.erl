-module(new_handler).
-export([alert_handler/2, log_handler/2]).

alert_handler(Type, Msg) ->
	io:format("~n*** ALERT! Type=~p, Message=~p ***~n", [Type, Msg]).

log_handler(Type, Msg) ->
	io:format("~n[LOG] (~p): ~p~n", [Type, Msg]).
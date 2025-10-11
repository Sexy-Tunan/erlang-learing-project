-module(event_handler).
-export([make/1, add_handler/2, event/2]).

make(Name) ->
	register(Name, spawn(fun() -> my_handle(fun no_op/0) end)).

%% nothing
add_handler(Name, Fun) ->
	Name ! {add, Fun}.

event(Name, Event) ->
	Name ! {event, Event}.

my_handle(Fun) ->
	receive
		{add, Fun1} ->
			my_handle(Fun1);
		{event, Any} ->
			catch
				Fun(Any),
			my_handle(Fun)
	end.
no_op() -> void.

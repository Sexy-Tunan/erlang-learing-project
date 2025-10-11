-module(hello).
-export([world/0]).
% c("src/hello", [{outdir,"ebin"}]). 

world() ->
    io:format("Hello, Erlang from VS Code!~n").
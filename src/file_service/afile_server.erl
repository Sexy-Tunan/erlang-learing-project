-module(afile_server). 
-export([start/1, loop/1]). 

start(Dir) -> spawn(afile_server, loop, [Dir]). 

loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File_Name}} ->
            Full = file_name:join(Dir, File_Name),
            Client ! {self(), file:read_file(Full)}
    end,
    loop(Dir). 
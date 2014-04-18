-module(cont_rd). 

-export([every/2, every_loop/2, read_file/1]). 

every(Filename, Time) ->
    spawn(fun() ->
                {ok, File} = file:open(Filename, read),
                every_loop(Time, File),
                {ok} = file:close(File)
        end).

every_loop(Time, File) ->
    receive
        {'EXIT', _Why} ->
            true
    after Time ->
            read_file(File),
            every_loop(Time, File)
    end. 

read_file(File) ->
    case io:get_line(File, '') of
         eof -> [];
         Data -> io:format("~s", [Data])
    end. 


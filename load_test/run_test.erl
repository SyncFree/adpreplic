-module(run_test).
-export([test/1]).

for_each_line_in_file(Name, Proc, Accum0) ->
    {ok, Device} = file:open(Name, [read]),
    for_each_line(Device, Proc, Accum0).
 
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                for_each_line(Device, Proc, NewAccum)
    end.

get_replica_from_dc(_DC) ->
    net_adm:ping(_DC).

get_replica_from_first_dc([]) ->
    {ok, "Finished"};

get_replica_from_first_dc([H | T]) ->
    case get_replica_from_dc(H) of
        pong  ->
            io:fwrite("Pong sucess from ~p \n", [H]),
            get_replica_from_first_dc(T);
        {error, _Info}   -> {ok, _Info}
    end
    .

trim_add_to_list(X, Y) ->
    Y ++ [erlang:list_to_atom(re:replace(X,"\n","",[{return,list}]))].

test(FilePath) ->
    {ok, Device} = file:open(FilePath, [read]),
    _Line = io:get_line(Device, ""),
    DCs = for_each_line_in_file("adpreplic-nodes.txt",
        fun (X,Y) -> trim_add_to_list(X, Y) end,
        []),
    {ok, Value} = get_replica_from_first_dc(DCs),
    io:fwrite(Value),
    file:close(FilePath).

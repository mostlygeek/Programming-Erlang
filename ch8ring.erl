%% Write a ring benchmark. Create N processes in a ring. 
%% Send a message round the ring M times so that a total 
%% of N * M messages get sent. Time how long this takes 
%% for different values of N and M.

-module(ch8ring).
-export([create/1,bench/2]).

-ifdef(debug).
-define(DEBUG(X,Y), io:format(X,Y)).
-else.
-define(DEBUG(X,Y), void).
-endif.

bench(N, M) ->
    First = create(N),
    statistics(wall_clock),
    First ! {send, t, M},
    io:format("Start ~p processes, ~p rounds = ~p msg passes.~n",[N,M,N*M]).

% make a ring with N nodes
create(N) ->
    First = spawn(fun() -> loop(defer) end),
    ?DEBUG(" - Created first node: ~p~n", [First]),    
    create(N-1, First, First),    
    First.
    
create(0, First, PassTo) ->
    ?DEBUG("msg to ~p to set passTo to ~p~n",[First,PassTo]),
    First ! {passTo, PassTo};
    
create(N, First, PassTo) ->
    Pid = spawn(fun() -> loop(PassTo) end),
    ?DEBUG(" - Created ~p with pass to ~p~n", [Pid, PassTo]),
    create(N-1, First, Pid). 

loop(PassTo) ->
    receive        
        % set the new PassTo, used to link process N to process 1.
        {passTo, NewPassTo} ->
            loop(NewPassTo);
            
        % sends a new message
        {send, Msg, M} -> 
            case (is_pid(PassTo)) of 
                true -> 
                    ?DEBUG("~p got message~n",[self()]),
                    PassTo ! {pass, self(), Msg, M, 0};
                false -> false
            end, 
            loop(PassTo);
        
        % pass message along
        {pass, Origin, Msg, M, Count} ->
            if 
                Origin =:= self() andalso Count < M ->
                    ?DEBUG("  DONE ROUND ~p of ~p~n",[Count+1,M]),
                    ?DEBUG("  > ~p pass [~p] to ~p~n",[self(), Msg, PassTo]),
                    PassTo ! {pass, Origin, Msg, M, Count+1};
                Origin =:= self() andalso Count =:= M ->
                    {_, Time} = statistics(wall_clock),
                    io:format("Done. Time: ~p seconds!~n", [Time/1000]),
                    Origin ! stop;                    
                true ->
                    ?DEBUG("  > ~p pass [~p] to ~p~n",[self(), Msg, PassTo]),
                    PassTo ! {pass, Origin, Msg, M, Count}
            end,
            loop(PassTo);
    
        % send a stop command down the line
        stop ->
            ?DEBUG("  ~p stopping~n",[self()]),
            PassTo ! stop,
            true
    end.
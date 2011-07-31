%% Write a ring benchmark. Create N processes in a ring. 
%% Send a message round the ring M times so that a total 
%% of N * M messages get sent. Time how long this takes 
%% for different values of N and M.

-module(ch8ring).
-export([create/1]).

% make a ring with N nodes
create(N) ->
    First = spawn(fun() -> loop(defer) end),
    io:format(" - Created first node: ~p~n", [First]),    
    create(N-1, First, First),    
    First.
    

create(0, First, PassTo) ->
    io:format("msg to ~p to set passTo to ~p~n",[First,PassTo]),
    First ! {passTo, PassTo};
    
create(N, First, PassTo) ->
    Pid = spawn(fun() -> loop(PassTo) end),
    io:format(" - Created ~p with pass to ~p~n", [Pid, PassTo]),
    create(N-1, First, Pid). 

loop(PassTo) ->
    receive        
        % set the next. used when we're at the end.
        {passTo, NewPassTo} ->
            loop(NewPassTo);
            
        % sends a new message
        {send, Msg, M} -> 
            case (is_pid(PassTo)) of 
                true -> 
                    io:format("~p got message~n",[self()]),
                    PassTo ! {pass, self(), Msg, M, 0};
                false -> false
            end, 
            loop(PassTo);
        
        {pass, Origin, Msg, M, Count} ->
            if 
                Origin =:= self() andalso Count < M ->
                    io:format("  DONE ROUND ~p of ~p~n",[Count+1,M]),
                    io:format("  > ~p pass [~p] to ~p~n",[self(), Msg, PassTo]),
                    PassTo ! {pass, Origin, Msg, M, Count+1};
                Origin =:= self() andalso Count =:= M ->
                    io:format("  All Done!.~n");
                true ->
                    io:format("  > ~p pass [~p] to ~p~n",[self(), Msg, PassTo]),
                    PassTo ! {pass, Origin, Msg, M, Count}
            end,
            loop(PassTo);

        stop ->
            io:format("  ~p stopping~n",[self()]),
            PassTo ! stop,
            true
    end.    
-module(edemo2). 
-export([start/2]). 

start(Bool, M) ->
    A = spawn(fun() -> a() end), 
    B = spawn(fun() -> b(A, Bool) end), 
    C = spawn(fun() -> c(B, M) end),
    sleep(1000),
    status(b, B),
    status(c, C).
    
a() ->
    process_flag(trap_exit, true),
    wait(a).

b(A, Bool) ->
    process_flag(trap_exit, Bool),
    link(A),
    wait(b).

c(B, M) ->
    process_flag(trap_exit, true), 
    link(B), 
    exit(B, M), 
    wait(c).

wait(Prog) ->
    receive 
        Any ->
            io:format("Process ~p recieved ~p~n",[Prog, Any]),
            wait(Prog)
    end.

sleep(T) ->
    receive
    after T -> true
    end.

status(Name, Pid) ->
    case erlang:is_process_alive(Pid) of 
        true ->
            io:format("process ~p (~p) is alive~n", [Name, Pid]);
        false ->
            io:format("process ~p (~p) is dead~n", [Name, Pid])
    end.
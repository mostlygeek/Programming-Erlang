-module(ch8ring).
-compile(export_all).

start(0) ->
    io:format("done~n");
    
start(N) ->
    io:format("~p left to create~n",[N]),
    start(N-1).


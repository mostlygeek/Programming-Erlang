-module(ti_server). 
-behaviour(gen_server). 

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []). 

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.
    %%% notice the timeout = 0, goes to handle_info(timeout... immediately
    %%% this allows the function to return immediately without the
    %%% caller having to wait 

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State)  ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State), 
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
   % *BLOCKS* until there is a connection 
   {ok, _Sock} = gen_tcp:accept(LSock),

   % immediately start another listener waiting for a connection
   ti_sup:start_child(),

   {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
handle_data(Socket, RawData, State) ->
    gen_tcp:send(Socket, RawData), 
    State.

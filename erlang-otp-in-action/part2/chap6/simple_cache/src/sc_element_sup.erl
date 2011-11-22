-module(sc_element_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE). 

%%% ============================================================================
%%% API Definition
%%% ============================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).


%%% ============================================================================
%%% Callbacks
%%% ============================================================================

%% the simple_on_for_one RestartStrategy is important. It creates a simple 
%% type of supervisor. Where start_child/2 can be called in a simplified form
%% which results in all children being spawned with the exact same definition
%%
%% the start_child/2 function takes a Value, LeaseTime, notice that 
%% supervisor:start_child/2 just takes these values...
init([]) ->
    Element = {sc_element, {sc_element, start_link, []},
               temporary, brutal_kill, worker, [sc_element]}, 
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1}, 
    {ok, {RestartStrategy, Children}}.



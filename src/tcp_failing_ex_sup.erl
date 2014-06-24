-module(tcp_failing_ex_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/2
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Port),     {I,  {I, start_link, [Port]}, permanent, 5000, Type, [I]}).
-define(CHILD(Id, I, Type, Port), {Id, {I, start_link, [Port]}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Name, Port) ->
    supervisor:start_child(?MODULE,
                           ?CHILD(Name, tcp_failing_srv, worker, Port)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [
            ?CHILD(tcp_failing_main_srv, worker,
                   tcp_failing_ex_app:get_port())
           ]} }.


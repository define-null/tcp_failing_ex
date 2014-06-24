-module(tcp_failing_ex_app).
-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).
-export([get_port/0,
         get_timeout/0
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tcp_failing_ex_sup:start_link().

stop(_State) ->
    ok.

start() ->
    application:start(sasl),
    application:start(tcp_failing_ex).

get_port() ->
    application:get_env(tcp_failing_ex, port, 8899).

get_timeout() ->
    application:get_env(tcp_failing_ex, producer_timeout, 5).

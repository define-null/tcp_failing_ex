%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(tcp_failing_main_srv).
-export([start_link/1, init/2]).

start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [self(), Port]).

init(Parent, Port) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    io:format("Listen ~p~n", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
                                        {active, false},
                                        {packet, raw},
                                        {reuseaddr, true},
                                        {nodelay, true}]),
    srv_loop(LSock).

srv_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("Accept client~n"),
    
    {ok, Pid} = tcp_failing_ex_sup:start_worker(make_ref(), Sock),
    gen_tcp:controlling_process(Sock, Pid),

    srv_loop(LSock).


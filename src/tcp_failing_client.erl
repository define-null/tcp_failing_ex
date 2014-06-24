%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(tcp_failing_client).

-export([test/0, test/2]).
-export([connect/2, connect/3, subscribe/1]).

-record(client, {sock,timeout}).
-define(DEF_CLIENT_TIMEOUT, 50000).

test() ->
    test("localhost", 8899).

test(Host, Port) ->
    spawn_link(fun() ->
                       io:format("connecting to ~p:~p~n", [Host, Port]),
                       {ok, C} = ?MODULE:connect(Host, Port),
                       ?MODULE:subscribe(C)
               end).

connect(Addr, Port) ->
    connect(Addr, Port, []).
connect(Addr, Port, Opts) ->
    case gen_tcp:connect(Addr, Port, connection_opts()) of
        {ok, Sock} ->
            TO = proplists:get_value(timeout, Opts, ?DEF_CLIENT_TIMEOUT),
            {ok, #client{sock=Sock, timeout=TO}};
        {error, _} = Err -> Err
    end.

subscribe(#client{sock=Sock} = State) ->
    ok = gen_tcp:send(Sock, term_to_binary({start_stream})),
    erlang:send(self(), ping),
    rcvloop(State).

%% ------------------------------------------------------------------
%% Test client internal
%% ----------------------------------------------------z--------------

rcvloop(#client{sock=Sock, timeout=TO} = State) ->
    receive
        ping ->
            inet:setopts(Sock, [{active, false}]),
            timer:sleep(20000),
            inet:setopts(Sock, [{active, true}]),
            
            io:format("~p before send~n", [calendar:now_to_local_time(os:timestamp())]),
            Res = gen_tcp:send(Sock, term_to_binary(ping)),
            io:format("~p after send ~p~n", [calendar:now_to_local_time(os:timestamp()), Res]),
            erlang:send_after(1000, self(), ping),
            rcvloop(State);
        {tcp, Sock, Data} ->
            try on_msg({msg, Data}),
                 rcvloop(State)
            catch
                error:E -> 
                    erlang:error({E, erlang:get_stacktrace()})
            end;
        Other ->
            on_msg(Other),
            {error, Other}
    after TO ->
            io:format("Timeout~n"),
            {error, timeout}
    end.

on_msg(pong) ->
    io:format("~p: pong~n", [current()]);
on_msg({msg, _M}) ->
    io:format("~p: msg~n", [current()]);
on_msg(M) ->
    io:format("~p: ~p~n", [current(), M]).

connection_opts() ->
    [binary, 
     {packet,    4}, 
     {reuseaddr, true}, 
     {nodelay,   true}, 
     {active,    true}
    ].

current() ->
    calendar:now_to_local_time(os:timestamp()).

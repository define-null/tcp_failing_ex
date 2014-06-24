%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(tcp_failing_srv).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(PAYLOAD, <<"payload: 11111111111111111111111111111111111111111111111">>).

%% API Function Exports

-export([start_link/1]).

%% gen_server Function Exports

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3, connection_opts/0]).

-export([producer_loop/2]).

start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).
    

%% gen_server Function Definitions

-record(state, {sock :: inet:socket(),
                streamers = [] :: [pid()]
               }).

init([Port]) ->
    io:format("~p~n", [Port]),
    self() ! {init_sock},
    {ok, #state{sock = Port}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({init_sock}, #state{sock=Socket} = State) ->
    io:format("Init sock ~p~n",[self()]),
    
    Pids = [begin spawn_producer(Socket) end || _N <- lists:seq(1, 40)],
    inet:setopts(Socket, connection_opts()),
    {noreply, State#state{streamers=Pids}};

handle_info({tcp, Socket, Data}, #state{sock=Socket} = State) ->
    try
        Msg = binary_to_term(Data, [safe]),
        io:format("~p: ~p ~p~n", [current(), self(), Msg]),
        
        State1 = reply(exec(State, Msg)),
        inet:setopts(Socket, [{active, once}]),
        {noreply, State1}
    catch
        Type:Reason ->
            error_logger:error_report([<<"Error processing request">>,
                                       {error, {Type, Reason}},
                                       {stack, erlang:get_stacktrace()}]),
            {stop, {Type, Reason}, State}
    end;

handle_info(TcpErrorOrClosed, #state{sock=Socket} = State) ->
    io:format("Error: ~p: ~p ~p~n", [current(), self(), TcpErrorOrClosed]),
    ok = gen_tcp:close(Socket),
    {stop, shutdown, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

exec(State = #state{streamers = Pids}, {start_stream}) ->
    [Pid ! start || Pid <- Pids],
    {State, undefined};
exec(State, ping) ->
    {State, pong}.

reply({State, undefined}) -> State;
reply({#state{sock=Socket} = State, Reply}) ->
    io:format("~p bsend: ~p~n",    [current(), self()]),
    Res = gen_tcp:send(Socket, term_to_binary(Reply)),
    io:format("~p asend: ~p ~p~n", [current(), self(), Res]),
    State.

spawn_producer(Port) ->
    proc_lib:spawn_link(fun() ->
                                Timeout = tcp_failing_ex_app:get_timeout(),
                                receive
                                    start ->
                                        producer_loop(Port, Timeout)
                                end
                        end).

producer_loop(Port, Timeout) ->
    timer:sleep(Timeout),
    case erlang:port_command(Port, ?PAYLOAD, [nosuspend]) of
        true  -> producer_loop(Port, Timeout);
        false -> producer_loop(Port, Timeout)
    end.

connection_opts() ->
    [binary, 
     {packet,    4}, 
     {reuseaddr, true}, 
     {nodelay,   true}, 
     {active,    once}
    ].

current() ->
    calendar:now_to_local_time(os:timestamp()).

%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <ptr@t440s>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2015 by Piotr Anielski <ptr@t440s>
%%%-------------------------------------------------------------------
-module(tcp_listener).

-export([start/1, init/1, accept_new_client/1]).

start(Port) ->
    spawn_link(?MODULE, init, [Port]).

%% stop() ->

init(Port) ->
    Opts = [binary, {packet, 0}, {active, false}, {keepalive, true}],
    {ok, ListeningSocket} = gen_tcp:listen(Port, Opts), %% TODO options
    loop(Port, ListeningSocket).

loop(Port, Socket) ->
    io:format("Waiting to accept on port: ~p...~n",[Port]),
    case gen_tcp:accept(Socket) of
        {ok, ClientSocket} ->
            io:format("Accepted~n",[]),
            Pid = spawn_link(?MODULE, accept_new_client, [ClientSocket]),
            gen_tcp:controlling_process(ClientSocket, Pid);
        Error ->
            io:format("Unexpected error while listening on port ~p: ~p~n", 
                      [Port, Error])
    end,
    loop(Port, Socket).


accept_new_client(Socket) ->
    %% BinaryMsg = do_receive(Socket),
    %% join and call chat_server:join(Name)
    JoinMsg = receive_msg(Socket),
    case JoinMsg of
        {join, Name} ->
            case chat_server:join(Name, Socket) of
                ok ->
                    wait_for_messages(Name, Socket);
                {error, name_already_taken} ->
                    send_error(Socket, name_already_taken),
                    wait_for_messages(Name, Socket);
                {error, Error} ->
                    send_error(Socket, Error)
            end;
        Error ->
            io:format("Unexpected error while waiting for join msg: ~p~n",
                      [Error])
    end.

wait_for_messages(Name, Socket) ->
    case receive_msg(Socket) of
        {msg, Msg} ->
            io:format("~p will say ~p~n", [Name, Msg]),
            chat_server:say(Name, Msg),
            wait_for_messages(Name, Socket);
        {leave} ->
            io:format("~p wants to leave~n", [Name]),
            chat_server:leave(Name);
        Error ->
            io:format("Unexpected error while listening for messages: ~p~n",
                      [Error])
    end.

send_error(Socket, Error) ->
    io:format("sending error ~p~n", [Error]),
    gen_tcp:send(Socket, term_to_binary(Error)).

receive_msg(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            binary_to_term(Bin);
        Error ->
            io:format("Unexpected error while receiving data: ~p~n", [Error]),
            Error
    end.

%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <ptr@t440s>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2015 by Piotr Anielski <ptr@t440s>
%%%-------------------------------------------------------------------
-module(tcp_listener).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([wait_for_clients/2, accept_new_client/1]).

-define(SERVER, ?MODULE).

-type socket() :: gen_tcp:socket().

-record(state, {listener :: pid(),
                listener_socket :: socket()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    Opts = [binary, {packet, 0}, {active, false}, {keepalive, true}],
    {ok, ListeningSocket} = gen_tcp:listen(Port, Opts), %% TODO options
    Pid = spawn_link(?MODULE, wait_for_clients, [Port, ListeningSocket]),
    {ok, #state{listener = Pid, listener_socket = ListeningSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    exit(State#state.listener, shutdown),
    gen_tcp:close(State#state.listener_socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec wait_for_clients(inet:port_number(), gen_tcp:socket()) -> no_return().
wait_for_clients(Port, Socket) ->
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
    wait_for_clients(Port, Socket).


accept_new_client(Socket) ->
    %% BinaryMsg = do_receive(Socket),
    %% join and call chat_server:join(Name)
    JoinMsg = receive_msg(Socket),
    case JoinMsg of
        {join, Name} ->
            join_chat(Socket, Name);
        Error ->
            io:format("Unexpected error while waiting for join msg: ~p~n",
                      [Error])
    end.

join_chat(Socket, Name) ->
    case chat_server:join(Name, Socket) of
        ok ->
            wait_for_messages(Name, Socket);
        {error, name_already_taken} ->
            send_error(Socket, name_already_taken),
            wait_for_messages(Name, Socket);
        {error, Error} ->
            send_error(Socket, Error)
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

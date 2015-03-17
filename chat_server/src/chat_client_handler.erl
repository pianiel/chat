%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <ptr@t440s>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2015 by Piotr Anielski <ptr@t440s>
%%%-------------------------------------------------------------------
-module(chat_client_handler).

-behaviour(gen_server).

%% API
-export([start_link/1, send_to_client/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type socket() :: gen_tcp:socket().
-type client_state() :: joined | ok.

-record(state, {listener :: pid(),
                listener_socket :: socket(),
                client_state :: client_state()}).

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
start_link([Socket]) ->

    io:format("starting client_handler~n"),

    gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).


receive_from_client(Pid, BinaryMessage) ->
    gen_server:cast(Pid, {recv, BinaryMessage}).


send_to_client(Pid, BinaryMessage) ->
    gen_server:cast(Pid, {send, BinaryMessage}).


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
init([Socket]) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> receive_message(Socket, self()) end),
    {ok, #state{listener = Pid,
                listener_socket = Socket,
                client_state = connected}}.

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
handle_cast({recv, BinaryMsg}, State) ->
    Msg = decode(BinaryMsg),
    NewState = handle_action(Msg, State),
    {noreply, NewState};
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
terminate(_Reason, _State) ->
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

decode(BinaryMessage) ->
    binary_to_term(BinaryMessage).


handle_action({join, Name}, State = #state{client_state = connected}) ->
    %% join, store name in state, switch state to joined
    State;
handle_action({join, Name}, State) ->
    %% cannot join twice
    State;
handle_action({leave}, State = #state{client_state = joined}) ->
    %% leave
    State;
handle_action({leave}, State) ->
    %% ignore leaving by user who hasn't joined
    State;
handle_action({say, Msg}, State) ->
    %% send to chat server so it will broadcast
    State.





wait_for_join_msg(Socket, Pid) ->
    JoinMsg = receive_message(Socket, Pid),
    case JoinMsg of
        {join, Name} ->
            join_chat(Socket, Name, Pid);
        Error ->
            io:format("Unexpected error while waiting for join msg: ~p~n",
                      [Error])
    end.

join_chat(Socket, Name, Pid) ->
    case chat_server:join(Name, Socket) of
        ok ->
            wait_for_messages(Name, Socket, Pid);
        {error, name_already_taken} ->
            send_error(Socket, name_already_taken),
            wait_for_join_msg(Socket, Pid);
        {error, Error} ->
            send_error(Socket, Error)
    end.

wait_for_messages(Name, Socket, Pid) ->
    case receive_message(Socket, Pid) of
        {say, Msg} ->
            io:format("~p will say ~p~n", [Name, Msg]),
            chat_server:say(Name, Msg),
            wait_for_messages(Name, Socket, Pid);
        {leave} ->
            io:format("~p wants to leave~n", [Name]),
            chat_server:leave(Name);
        {join, OtherName} ->
            io:format("~p tries to join again on the same conn", [OtherName]),
            wait_for_messages(Name, Socket, Pid);
        Error ->
            io:format("Unexpected error while listening for messages: ~p~n",
                      [Error])
    end.

send_error(Socket, Error) ->
    io:format("sending error ~p~n", [Error]),
    gen_tcp:send(Socket, term_to_binary(Error)).

receive_message(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            receive_from_client(Pid, Bin),
            receive_message(Socket, Pid);
        {error, Reason} ->
            io:format("Error while receiving data: ~p~n", [Reason]),
            {error, Reason}
    end.

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
-type client_state() :: joined | connected.

-record(state, {listener :: pid(),
                listener_socket :: socket(),
                client_name :: binary(),
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

    gen_server:start_link(?MODULE, [Socket], []).


receive_from_client(Pid, BinaryMessage) ->
    gen_server:cast(Pid, {recv, BinaryMessage}).


send_to_client(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).


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
    Myself = self(),
    Pid = spawn_link(fun() -> receive_message(Socket, Myself) end),
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
handle_cast({recv, closed_socket}, State) ->
    Msg = {leave},
    _State = handle_action(Msg, State),
    {stop, normal, State};
handle_cast({recv, BinaryMsg}, State) ->
    Msg = decode(BinaryMsg),
    NewState = handle_action(Msg, State),
    {noreply, NewState};
handle_cast({send, Msg}, State) ->
    send_message(Msg, State),
    {noreply, State};
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

encode(Message) ->
    term_to_binary(Message).

decode(BinaryMessage) ->
    binary_to_term(BinaryMessage).


handle_action({join, Name}, State = #state{client_state = connected}) ->
    %% join, store name in state, switch state to joined
    case chat_server:join(Name) of
        ok ->
            send_message(ok, State),
            State#state{client_state = joined, client_name = Name};
        {error, Reason} ->
            send_message({error, Reason}, State),
            State
    end;
handle_action({join, _Name}, State) ->
    %% cannot join twice, send error
    State;
handle_action({leave}, State = #state{client_state = joined}) ->
    chat_server:leave(State#state.client_name),
    State#state{client_state = connected};
handle_action({leave}, State) ->
    %% ignore leaving by user who wasn't joined
    State;
handle_action({say, Msg}, State) ->
    chat_server:say(State#state.client_name, Msg),
    State.


send_message(Message, State) ->
    BinaryMessage = encode(Message),
    gen_tcp:send(State#state.listener_socket, BinaryMessage).


receive_message(Socket, Pid) ->

    io:format("waiting for messages~n"),

    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->

            io:format("received message ~p~n", [Bin]),
            io:format("casting to pid: ~p~n", [Pid]),

            receive_from_client(Pid, Bin),
            receive_message(Socket, Pid);
        {error, closed} ->
            receive_from_client(Pid, closed_socket);
        {error, Reason} ->
            io:format("Error while receiving data: ~p~n", [Reason]),
            {error, Reason}
    end.

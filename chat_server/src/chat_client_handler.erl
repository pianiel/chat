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
-type encoded_message() :: binary().
-type message() :: ok | tuple().

-record(state, {listener :: pid(),
                listener_socket :: socket(),
                client_name :: binary(),
                client_state :: client_state()}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(socket()) -> {ok, pid()} | ignore | {error, any()}.
start_link([Socket]) ->
    gen_server:start_link(?MODULE, [Socket], []).

-spec receive_from_client(pid(), encoded_message() | closed_socket) -> ok.
receive_from_client(Pid, BinaryMessage) ->
    gen_server:cast(Pid, {recv, BinaryMessage}).

-spec send_to_client(pid(), message()) -> ok.
send_to_client(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([inet:port()]) -> {ok, state()}.
init([Socket]) ->
    process_flag(trap_exit, true),
    Myself = self(),
    Pid = spawn_link(fun() -> receive_message(Socket, Myself) end),
    {ok, #state{listener = Pid,
                listener_socket = Socket,
                client_state = connected}}.

-spec handle_call(Request :: any(), any(), state()) ->
                         {reply, Reply :: any(), state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Msg :: any(), state()) -> {noreply, state()} |
                                            {stop, normal, state()}.
handle_cast({recv, closed_socket}, State) ->
    Msg = {leave},
    handle_action(Msg, State),
    {stop, normal, State};
handle_cast({recv, BinaryMsg}, State) ->
    try decode(BinaryMsg) of
        Msg ->
            NewState = handle_action(Msg, State),
            {noreply, NewState}
    catch
        error:badarg ->
            {noreply, State}
    end;
handle_cast({send, Msg}, State) ->
    send_message(Msg, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: any(), state()) -> {noreply, state()} |
                                             {stop, Reason :: any(), state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: any(), state()) -> ok.
terminate(_Reason, State = #state{client_state = joined}) ->
    %% in case of an internal error, remove client from chat manually
    Msg = {leave},
    handle_action(Msg, State),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: any(), state(), Extra :: any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec encode(message()) -> encoded_message().
encode(Message) ->
    term_to_binary(Message).

-spec decode(encoded_message()) -> message().
decode(BinaryMessage) ->
    binary_to_term(BinaryMessage).

-spec handle_action(Msg :: message(), state()) -> state().
handle_action({join, Name}, State = #state{client_state = connected}) ->
    case chat_server:join(Name) of
        ok ->
            send_message(ok, State),
            State#state{client_state = joined, client_name = Name};
        {error, Reason} ->
            send_message({error, Reason}, State),
            State
    end;
handle_action({join, _Name}, State) ->
    %% cannot join twice
    send_message({error, user_already_joined}, State),
    State;
handle_action({leave}, State = #state{client_state = joined}) ->
    chat_server:leave(State#state.client_name),
    State#state{client_state = connected};
handle_action({leave}, State) ->
    %% ignore leaving of user who hasn't joined
    State;
handle_action({say, _Msg}, State = #state{client_state = connected}) ->
    send_message({error, user_has_not_joined}, State);
handle_action({say, Msg}, State) ->
    chat_server:say(State#state.client_name, Msg),
    State;
handle_action(_Other, State) ->
    %% ignore
    State.


-spec send_message(message(), state()) -> ok | {error, Reason :: any()}.
send_message(Message, State) ->
    BinaryMessage = encode(Message),
    gen_tcp:send(State#state.listener_socket, BinaryMessage).

-spec receive_message(socket(), pid()) -> ok | {error, Reason :: any()}.
receive_message(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            receive_from_client(Pid, Bin),
            receive_message(Socket, Pid);
        {error, closed} ->
            receive_from_client(Pid, closed_socket);
        {error, Reason} ->
            io:format("Error while receiving data: ~p~n", [Reason]),
            {error, Reason}
    end.

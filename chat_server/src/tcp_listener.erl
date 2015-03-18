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

-define(SERVER, ?MODULE).

-type socket() :: gen_tcp:socket().

-record(state, {listener :: pid(),
                listener_socket :: socket()}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(inet:port()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([inet:port()]) -> {ok, state()}.
init([Port]) ->
    Opts = [binary, {packet, 2}, {active, false}, {keepalive, true}],
    {ok, ListeningSocket} = gen_tcp:listen(Port, Opts),
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> wait_for_clients(Port, ListeningSocket) end),
    {ok, #state{listener = Pid, listener_socket = ListeningSocket}}.


-spec handle_call(Request :: any(), any(), state()) ->
                         {reply, Reply :: any(), state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Msg :: any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: any(), state()) -> {noreply, state()} |
                                             {stop, Reason :: any(), state()}.
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: any(), state()) -> ok.
terminate(_Reason, State) ->
    exit(State#state.listener, shutdown),
    gen_tcp:close(State#state.listener_socket),
    ok.

-spec code_change(OldVsn :: any(), state(), Extra :: any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec wait_for_clients(inet:port_number(), gen_tcp:socket()) -> no_return().
wait_for_clients(Port, Socket) ->
    case gen_tcp:accept(Socket) of
        {ok, ClientSocket} ->
            chat_client_sup:create_client(ClientSocket);
        Error ->
            io:format("Unexpected error while listening on port ~p: ~p~n",
                      [Port, Error])
    end,
    wait_for_clients(Port, Socket).

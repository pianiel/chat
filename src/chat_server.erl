%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <pr.anielski@gmail.com>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%% A simple chat server module
%%% @end
%%% Created : 16 Mar 2015 by Piotr Anielski <pr.anielski@gmail.com>
%%%-------------------------------------------------------------------
-module(chat_server).

-behaviour(gen_server).

%% API
-export([start_link/0, join/1, say/2, leave/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type user_name() :: binary().
-type encoded_message() :: binary().
-type message() :: ok | tuple().

-record(state, {clients :: dict:dict(), backslashers :: sets:set()}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec join(user_name()) -> ok | {error, name_already_taken}.
join(Name) ->
    gen_server:call(?SERVER, {join, Name, self()}).

-spec say(user_name(), encoded_message()) -> ok.
say(Name, Msg) ->
    gen_server:cast(?SERVER, {say, Name, Msg}).

-spec leave(user_name()) -> ok.
leave(Name) ->
    gen_server:cast(?SERVER, {leave, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([any()]) -> {ok, state()}.
init([]) ->
    {ok, #state{clients = dict:new(), backslashers = sets:new()}}.


-spec handle_call(Request :: any(), any(), state()) ->
                         {reply, Reply :: any(), state()} |
                         {stop, normal, Reply :: any(), state()}.
handle_call({join, Name, Pid}, _From, State) ->
    {Reply, Clients} = handle_join(Name, Pid, State#state.clients),
    {reply, Reply, State#state{clients = Clients}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(Msg :: any(), state()) -> {noreply, state()}.
handle_cast({say, Name, <<"\\", Rest/binary>>}, State) ->
    %% if the message starts with '\', store the name so we know who tries to
    %% exploit our humble chat server and act like nothing happened
    NewState = handle_curious_people(Name, Rest, State),
    {noreply, NewState};
handle_cast({say, Name, Msg}, State) ->
    Message = {message, Name, Msg},
    broadcast_message(Message, State#state.clients),
    {noreply, State};
handle_cast({leave, Name}, State) ->
    Clients = dict:erase(Name, State#state.clients),
    Unpresence = {unpresence, Name},
    broadcast_message(Unpresence, Clients),
    {noreply, State#state{clients = Clients}};
handle_cast(_Msg, State) ->
    {noreply, State}.


-spec handle_info(Info :: any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(Reason :: any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: any(), state(), Extra :: any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_join(user_name(), pid(), dict:dict()) ->
                         {ok | {error, name_already_taken}, dict:dict()}.
handle_join(Name, Pid, Clients) ->
    case dict:is_key(Name, Clients) of
        false ->
            Presence = {presence, Name},
            NewClients = dict:store(Name, Pid, Clients),
            broadcast_message(Presence, NewClients),
            {ok, NewClients};
        _ ->
            {{error, name_already_taken}, Clients}
    end.

-spec broadcast_message(message(), dict:dict()) -> ok.
broadcast_message(Msg, ClientsDict) ->
    [chat_client_handler:send_to_client(Pid, Msg)
     || {_ClientName, Pid} <- dict:to_list(ClientsDict)],
    ok.

-spec handle_curious_people(user_name(), binary(), state()) -> state().
handle_curious_people(Name, _Msg, State) ->
    NewSet = sets:add_element(Name, State#state.backslashers),
    State#state{backslashers = NewSet}.

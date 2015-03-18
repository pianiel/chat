%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <ptr@t440s>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2015 by Piotr Anielski <ptr@t440s>
%%%-------------------------------------------------------------------
-module(chat_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, create_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec create_client(gen_tcp:socket()) -> {ok, pid()}.
create_client(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [[Socket]]),
    gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 50,
    MaxSecondsBetweenRestarts = 100,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [client_child_spec()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec client_child_spec() -> tuple().
client_child_spec() ->
    Restart = temporary,
    Shutdown = 2000,
    Type = worker,
    ClientModule = chat_client_handler,

    {ClientModule, {ClientModule, start_link, []},
     Restart, Shutdown, Type, [ClientModule]}.

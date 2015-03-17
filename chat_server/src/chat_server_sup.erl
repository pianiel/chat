-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    RestartStrategy = rest_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChatServerMod = chat_server,
    ChatServerChild = {ChatServerMod, {ChatServerMod, start_link, []},
                       Restart, Shutdown, Type, [ChatServerMod]},
    TcpListenerMod = tcp_listener,
    TcpListenerChild = {TcpListenerMod, {TcpListenerMod, start_link, [Port]},
                        Restart, Shutdown, Type, [TcpListenerMod]},
    {ok, {SupFlags, [ChatServerChild, TcpListenerChild]}}.

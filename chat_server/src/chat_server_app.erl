-module(chat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = 6667, %% can be passed as a parameter if needed
    chat_server_sup:start_link(Port).

stop(_State) ->
    ok.

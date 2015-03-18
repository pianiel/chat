-module(chat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(any(), any()) -> {ok, pid()} |
                             {ok, pid(), any()} |
                             {error, Reason :: any()}.
start(_StartType, _StartArgs) ->
    Port = 6667, %% can be passed as a parameter if needed
    chat_server_sup:start_link(Port).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

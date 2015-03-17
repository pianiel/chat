%%%-------------------------------------------------------------------
%%% @author Piotr Anielski <ptr@t440s>
%%% @copyright (C) 2015, Piotr Anielski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2015 by Piotr Anielski <ptr@t440s>
%%%-------------------------------------------------------------------
-module(chat_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(value(Key,Config), proplists:get_value(Key,Config)).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:start(chat_server),
    Timeout = 2000,
    Host = "localhost",
    Port = 6667,
    Opts = [binary, {packet, 2}, {active, false}],
    [{timeout, Timeout}, {host, Host}, {port, Port}, {opts, Opts} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(chat_server),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [check_join,
     check_failed_join,
     check_join_twice,
     check_send_message_without_joining,
     check_send_message_to_self,
     check_send_message_to_other,
     check_send_control].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
check_join(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    leave(Alice),
    close(Alice).

check_join_twice(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    %% second join
    gen_tcp:send(Alice, encode({join, Name})),

    assert_error(receive_msg(Alice, Config)),

    leave(Alice),
    close(Alice).


check_failed_join(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    Alice2 = join(Name, Config),
    R = receive_msg(Alice2, Config),
    assert_error(R),

    leave(Alice),
    close(Alice),
    close(Alice2).

check_send_message_without_joining(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    {ok, User} = gen_tcp:connect(?value(host, Config),
                                 ?value(port, Config),
                                 ?value(opts, Config)),
    Msg = <<"huh?">>,
    say(User, Msg),

    assert_timeout(Alice, Config),

    R = receive_msg(User, Config),
    assert_error(R),
    leave(Alice),
    close(Alice),
    close(User).


check_send_message_to_self(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    Msg = <<"hello">>,
    say(Alice, Msg),
    R = receive_msg(Alice, Config),

    assert_message(R, Name, Msg),

    leave(Alice),
    close(Alice).

check_send_message_to_other(Config) ->
    NameA = <<"Alice">>,
    Alice = join(NameA, Config),
    ok = receive_msg(Alice, Config),

    % presence Alice
    PresA = receive_msg(Alice, Config),
    assert_presence(PresA, NameA),

    NameB = <<"Bob">>,
    Bob = join(NameB, Config),
    ok = receive_msg(Bob, Config),

    %% presence Bob to Alice
    PresB = receive_msg(Alice, Config),
    assert_presence(PresB, NameB),
    %% presence Bob to Bob
    PresB2 = receive_msg(Bob, Config),
    assert_presence(PresB2, NameB),

    Msg = <<"hello bob">>,
    say(Alice, Msg),

    B1 = receive_msg(Bob, Config),
    assert_message(B1, NameA, Msg),

    A1 = receive_msg(Alice, Config),
    assert_message(A1, NameA, Msg),

    leave(Alice),
    %% unpresence of alice to bob
    UnpresA = receive_msg(Bob, Config),
    assert_unpresence(UnpresA, NameA),

    leave(Bob),
    close(Alice),
    close(Bob).

check_send_control(Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    Msg = <<"\\hello">>,
    say(Alice, Msg),
    assert_timeout(Alice, Config),

    leave(Alice),
    close(Alice).


%%--------------------------------------------------------------------
%% helper functions
%%--------------------------------------------------------------------

encode(Msg) ->
    term_to_binary(Msg).

decode(Bin) ->
    binary_to_term(Bin).

close(Socket) ->
    gen_tcp:close(Socket).

join(Name, C) ->
    {ok, Socket} = gen_tcp:connect(?value(host, C),
                                   ?value(port, C),
                                   ?value(opts, C)),
    gen_tcp:send(Socket, encode({join, Name})),
    Socket.

leave(Socket) ->
    gen_tcp:send(Socket, encode({leave})),
    ok.

say(Socket, Msg) ->
    gen_tcp:send(Socket, encode({say, Msg})).

receive_msg(Socket, C) ->
    {ok, Result} = gen_tcp:recv(Socket, 0, ?value(timeout, C)),
    decode(Result).

assert_timeout(Socket, C) ->
    {error, timeout} = gen_tcp:recv(Socket, 0, ?value(timeout, C)).

assert_message(Result, Name, Msg) ->
    {message, Name, Msg} = Result.

assert_error(Result) ->
    {error, _Reason} = Result.

assert_presence(Result, Name) ->
    {presence, Name} = Result.

assert_unpresence(Result, Name) ->
    {unpresence, Name} = Result.

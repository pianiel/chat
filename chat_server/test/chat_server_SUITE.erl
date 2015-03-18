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

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:start(chat_server),
    Timeout = 2000,
    Host = "localhost",
    Port = 6667,
    Opts = [binary, {packet, 2}, {active, false}],
    [{timeout, Timeout}, {host, Host}, {port, Port}, {opts, Opts} | Config].

end_per_suite(_Config) ->
    application:stop(chat_server),
    ok.


init_per_testcase(_TestCase, Config) ->
    Name = <<"Alice">>,
    Alice = join(Name, Config),
    ok = receive_msg(Alice, Config),

    % presence Alice
    Pres = receive_msg(Alice, Config),
    assert_presence(Pres, Name),

    [{alice, Alice}, {alice_name, Name} | Config].


end_per_testcase(_TestCase, Config) ->
    Alice = ?value(alice, Config),
    leave(Alice),
    close(Alice).

all() ->
    [check_join,
     check_failed_join,
     check_join_twice,
     check_send_message_without_joining,
     check_send_message_to_self,
     check_send_message_to_other,
     check_send_control].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

check_join(_Config) ->
    ok. %% init per testcase + end per testcase


check_join_twice(Config) ->
    Alice = ?value(alice, Config),
    Name = ?value(alice_name, Config),

    %% second join
    gen_tcp:send(Alice, encode({join, Name})),

    assert_error(receive_msg(Alice, Config)),

    leave(Alice),
    close(Alice).


check_failed_join(Config) ->
    Name = ?value(alice_name, Config),

    Alice2 = join(Name, Config),
    R = receive_msg(Alice2, Config),
    assert_error(R),
    close(Alice2).


check_send_message_without_joining(Config) ->
    Alice = ?value(alice, Config),

    {ok, User} = gen_tcp:connect(?value(host, Config),
                                 ?value(port, Config),
                                 ?value(opts, Config)),
    Msg = <<"huh?">>,
    say(User, Msg),

    assert_timeout(Alice, Config),

    R = receive_msg(User, Config),
    assert_error(R),
    close(User).


check_send_message_to_self(Config) ->
    Alice = ?value(alice, Config),
    Name = ?value(alice_name, Config),

    Msg = <<"hello">>,
    say(Alice, Msg),
    R = receive_msg(Alice, Config),

    assert_message(R, Name, Msg).


check_send_message_to_other(Config) ->
    Alice = ?value(alice, Config),
    NameA = ?value(alice_name, Config),

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

    leave(Bob),
    %% unpresence of bob to alice
    UnpresB = receive_msg(Alice, Config),
    assert_unpresence(UnpresB, NameB),

    close(Bob).

check_send_control(Config) ->
    Alice = ?value(alice, Config),

    Msg = <<"\\hello">>,
    say(Alice, Msg),
    assert_timeout(Alice, Config).


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

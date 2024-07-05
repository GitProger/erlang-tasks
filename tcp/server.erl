-module(server).
-export([main/1]).

user_session(Sock) ->
  {ok, Got} = gen_tcp:recv(Sock, 0),
  gen_tcp:send(Sock, list_to_binary(io_lib:format("~p got ~s", [self(), Got]))),
  user_session(Sock).

loop(Serv) ->
  {ok, Conn} = gen_tcp:accept(Serv),
  spawn(fun () -> user_session(Conn) end),
  loop(Serv).

main(Port) ->
  {ok, S} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
  loop(S).

%kovkuf.int.spb.ru
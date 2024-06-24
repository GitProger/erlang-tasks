-module(db).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% db:new() -> Pid.
%% db:destroy(Pid) -> ok.
%% db:write(Key, Element, Pid) -> ok.
%% db:delete(Key, Pid) -> ok.
%% db:read(Key, Pid) -> {ok, Element} | {error, Reason}.
%% db:match(Element, Pid) -> [Key1, ..., KeyN].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-define(LIMIT, 100).

loop(State) ->
  receive
    {Client, kill}      -> Client ! ok, loop(maps:new());
    {Client, set, K, V} -> Client ! ok, loop(maps:put(K, V, State));
    {Client, del, K}    -> Client ! ok, loop(maps:remove(K, State));
    {Client, get, K} ->
      Client ! maps:get(K, State),
      loop(State);
    {Client, match, V} ->
      Client ! [Key || {Key, Val} <- maps:to_list(State), V =:= Val],
      loop(State);
    {Client, _} -> Client ! {error, invalid_request};
    _ -> {error, unknown_error}
  end.

new() -> spawn(fun () -> loop(maps:new()) end).

obtain() -> receive R -> R after ?LIMIT -> {error, connection_timeout} end.

destroy(Pid)     -> Pid ! {self(), kill},      obtain().
write(K, V, Pid) -> Pid ! {self(), set, K, V}, obtain().
delete(K, Pid)   -> Pid ! {self(), del, K},    obtain().
read(K, Pid)     -> Pid ! {self(), get, K},    obtain().
match(V, Pid)    -> Pid ! {self(), match, V},  obtain().

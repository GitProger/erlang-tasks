-module(proc).
-author("alex").
-export([
  p_map/2, p_map_v2/2,
  p_map_bad/2
]).

p_map_bad(_, []) -> [];
p_map_bad(F, [H | T]) ->
  Pid = self(), spawn(fun () -> Pid ! F(H) end),
  receive R -> [R | p_map(F, T)] end.

p_map_x(_, [], _) -> [];
p_map_x(F, [H | T], Fst) ->
  Pid = self(),
  spawn(fun () -> Pid ! F(H) end),
  TR = p_map_x(F, T, false),
  receive
    R -> if
           Fst -> lists:reverse([R | TR]);
           true -> [R | TR]
         end
%%    R -> case Fst of
%%           true -> lists:reverse([R | TR]);
%%           false -> [R | TR]
%%         end
  end.

p_map(F, L) -> p_map_x(F, L, true).
% c(proc), proc:p_map(fun (X) -> X * 2 end, [1, 2, 3]).
% c(proc), proc:p_map_v2(fun (X) -> X * 2 end, [1, 2, 3]).

%%indexed_t([H | T], C, R) -> indexed_t(T, C + 1, [{C, H} | R]).
%%indexed(L) -> lists:reverse(indexed_t(L, 0, [])).

indexed([], _) -> [];
indexed([H | T], C) -> [{C, H} | indexed(T, C + 1)].
indexed(L) -> indexed(L, 0).

p_map_v2(F, L) ->
  Pid = self(),
  [spawn(fun () -> Pid ! {I, F(X)} end)
    || {I, X} <- indexed(L)],
  maps:values(
    lists:foldl(fun (_, M) ->
                  receive {I, V} -> maps:put(I, V, M) end
                end, maps:new(), L)).
  %%%% imperative: ~~
  %%  M = maps:new(),
  %%  [receive {I, V} -> maps:set(I, V, M) end || _ <- L],
  %%  maps:values(M).


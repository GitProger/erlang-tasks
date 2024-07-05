-module(server).
-author("alex").

-export([connect/0, calculate/2]).
-define(LIMIT, 100).

foldl1(Fn, [H | T]) -> lists:foldl(Fn, H, T).

evaluate({add, Args}) -> lists:foldl(recursive(fun (A, B) -> A + B end), 0, Args);
evaluate({mul, Args}) -> lists:foldl(recursive(fun (A, B) -> A * B end), 1, Args);
evaluate({sub, Args}) -> foldl1(recursive(fun (A, B) -> A - B end), Args);
evaluate({'div', Args}) ->
  foldl1(
    recursive(fun (A, B) -> case B of 0 -> {error, division_by_zero}; _ -> A / B end end),
    Args);

evaluate(_) -> {error, unknown_operation}.

calc(P) when is_number(P) -> P;
calc(P) when is_tuple(P) -> evaluate(P).

% rec fn = fn `on` calc
% rec = flip on calc
%%recursive(Fn) -> fun (A, B) -> Fn(calc(A), calc(B)) end.
recursive(Fn) -> fun (A, B) ->
                  case calc(A) of {error, E} -> {error, E};
                    A_ -> case calc(B) of {error, E} -> {error, E};
                           B_ -> Fn(A_, B_)
                         end end end.

loop() -> receive {To, Msg} -> To ! evaluate(Msg), loop() end.
connect() -> spawn(fun loop/0).

obtain() -> receive R -> R after ?LIMIT -> {error, connection_timeout} end.
calculate(Pid, Arg) -> Pid ! {self(), Arg}, obtain().

%%c(server).
%%server:calculate(server:connect(), {mul, [2, {add, [5, 6]}]}).
%%
%%server:calculate(server:connect(), {add, [1, 2]}).
%%server:calculate(server:connect(), {'div', [1, 0]}).

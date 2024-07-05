-module(serv_test).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
  serv:start_link(),
  Pid = serv:connect(),
  ?assertEqual(3, serv:calculate(Pid, {add, [1, 2]})),
  Pid ! exit.

simple_test() ->
  serv:start_link(),
  Pid = serv:connect(),
  ?assertEqual(130, serv:calculate(Pid, {add, [10, {mul, [1, 2, 3, 4, 5]}]})),
  Pid ! exit.

zero_div_test() -> serv:start_link(),
  Pid = serv:connect(),
  ?assertEqual({error, division_by_zero}, serv:calculate(Pid, {'div', [1, 0]})),
  Pid ! exit.

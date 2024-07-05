-module(serv).
-behavior(gen_server).

-export([start_link/0, connect/0, calculate/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
connect() -> gen_server:call(?MODULE, conn).
calculate(Pid, Query) -> gen_server:call(Pid, {calc, Query}).
init(_Args) -> {ok, #state{}}.

handle_call(conn, _From, State) -> {reply, self(), State};
handle_call({calc, Action}, _From, State) ->
  try
    Res = evaluate(Action),
    {reply, Res, State}
  catch
    _:Reason ->
      {reply, {error, Reason}, State}
  end.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


match(P) when is_number(P) -> P;
match(P) when is_tuple(P) -> evaluate(P).

foldl1(Fn, [H | T]) -> lists:foldl(Fn, H, T).

recursive(Fn) -> fun (A, B) ->
  case match(A) of {error, E} -> {error, E};
    A_ -> case match(B) of {error, E} -> {error, E};
            B_ -> Fn(A_, B_)
          end end end.

evaluate(X) when is_number(X) -> X;
evaluate({add, Args}) -> lists:foldl(recursive(fun (A, B) -> A + B end), 0, Args);
evaluate({mul, Args}) -> lists:foldl(recursive(fun (A, B) -> A * B end), 1, Args);
evaluate({sub, Args}) -> foldl1(recursive(fun (A, B) -> A - B end), Args);
evaluate({'div', Args}) -> foldl1(recursive(fun (A, B) ->
  case B of 0 -> {error, division_by_zero}; _ -> A / B end
                                            end), Args);
evaluate(_) -> {error, unknown_operation}.


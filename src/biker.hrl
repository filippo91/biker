-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.
-define(ROUND_DURATION, 10000).
-define(N_ROUND, 1).
-define(N_BIKER, 0).
-define(DISTANCE, 100).

-module(beb).
-compile(export_all).
-include("msgy.hrl").
-include("biker.hrl").

broadcast_decision(Decision, Round) ->
    biker_repository:save_decision(Decision#decision.bikerId, Round, Decision).

receive_decisions(NumOfBikers, Round, NumOfBikers, Decisions) ->
    io:format("done.~n"),
    NumOfBikers, Round, NumOfBikers, 
    Decisions;

receive_decisions(BikerId, Round, NumOfBikers, Decisions) ->
    {OperationState, RDecision} = biker_repository:get_decision(BikerId, Round),
%    io:format("wait for ~B Round ~B, operation ~p, ~p~n", [BikerId, Round, OperationState, Status]),
    io:format("waiting for the other playes...~B ~B", [BikerId, Round]),
    if OperationState == ok, RDecision == not_found ->
            io:format(".."),
            timer:sleep(1000),
            receive_decisions(BikerId, Round, NumOfBikers, Decisions);
        true ->
            Decision = add_timestamp(RDecision),
            NewSet = Decisions ++ [Decision],
            receive_decisions(BikerId+1, Round, NumOfBikers, NewSet)
    end.

add_timestamp(Dec) ->
    ?PRINT(Dec),
    Current = calendar:local_time(), 
    TS = calendar:datetime_to_gregorian_seconds(Current),
    decision_repository:create_decision(Dec#decision.bikerId, Dec#decision.strategy, Dec#decision.player, Dec#decision.speed, TS).

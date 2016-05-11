-module(beb).
-compile(export_all).
-include("msgy.hrl").
-include("biker.hrl").

broadcast_decision(UpdatedStatus, Round) ->
    biker_repository:save_status(UpdatedStatus#status.id, Round, UpdatedStatus).

receive_decisions(NumOfBikers, Round, NumOfBikers, View) ->
	NumOfBikers, Round, NumOfBikers, 
    View;

receive_decisions(BikerId, Round, NumOfBikers, View) ->
    {OperationState, Status} = biker_repository:get_status(BikerId, Round),
    io:format("wait for ~B Round ~B, operation ~p, ~p~n", [BikerId, Round, OperationState, Status]),
    if OperationState == ok, Status == not_found ->
            timer:sleep(10000),
            receive_decisions(BikerId, Round, NumOfBikers, View);
        true ->
            NewView = View ++ [Status],
            receive_decisions(BikerId+1, Round, NumOfBikers, NewView)
    end.

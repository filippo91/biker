-module(tob).
-compile(export_all).
-include("msgy.hrl").
-include("biker.hrl").

broadcast_decision(UpdatedStatus, Round) ->
    [biker_repository:save_status(NewStatus#status.id, Round, NewStatus) || NewStatus <- UpdatedStatus].

receive_decisions(NumOfBikers, Round, NumOfBikers, Decisions) ->
	NumOfBikers, Round, NumOfBikers, 
    Decisions;

receive_decisions(BikerId, Round, NumOfBikers, Decisions) ->
    {OperationState, Decision} = biker_repository:get_decision(BikerId, Round),
    io:format("wait for ~B Round ~B, operation ~p, ~p~n", [BikerId, Round, OperationState, Decision]),
    if OperationState == ok, Decision == not_found ->
            timer:sleep(10000),
            receive_decisions(BikerId, Round, NumOfBikers, Decisions);
        true ->
            NewSet = Decisions ++ [Decision],
            receive_decisions(BikerId+1, Round, NumOfBikers, NewSet)
    end.

new_round(MasterId, Round) ->
    biker_repository:set_master_notification(MasterId, Round).  

create_initial_instance(NumOfBikers, Round, NumOfBikers) -> 
    Round, NumOfBikers;

create_initial_instance(BikerId, Round, NumOfBikers) ->
    InitialStatus = status_repository:create_status(BikerId),
    biker_repository:save_status(BikerId, Round, InitialStatus),
    create_initial_instance(BikerId+1, Round, NumOfBikers).     

decide_order(Decisions, Round) ->
	SortedDecisions = sort(Decisions),
	Solved = [solve_conflicts(Decision, Round) || Decision <- SortedDecisions],
    Solved.

sort(Decisions) ->
    lists:sort(
        fun(A, B) -> 
            if A#decision.ts =< B#decision.ts -> true;
                true -> false
            end
         end,
         Decisions).

solve_conflicts(Decision, Round) ->
    ?PRINT(Decision),
	BikerId = Decision#decision.bikerId,
	Biker = biker_repository:get_status(BikerId, Round),
	PlayerBehindId = Decision#decision.player,
    {ok,PlayerBehindDecision} = biker_repository:get_decision(PlayerBehindId, Round), 
	if  PlayerBehindDecision#decision.strategy == behind,
        PlayerBehindDecision#decision.player == Biker#status.id,
        Decision#decision.ts > PlayerBehindDecision#decision.ts ->
        % there is a conflict
      	% lose: try to mantain the previous speed
           	Speed = game_rules:validate_speed(Biker#status.energy, Biker#status.speed),
			NewDecision = decision_tob_repository:create_decision(BikerId, myself, 0, Speed, Decision#decision.ts),
            NewDecision;
    	true ->
            % win: keep the decision
 			Decision
	end.

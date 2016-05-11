-module(game_rules).
-compile(export_all).
-include("biker.hrl").
-include("msgy.hrl").

get_user_decision(BikerId, Round, NumOfBikers) ->
    StillEnergy = check_if_energy(BikerId, Round),
    if StillEnergy == true ->
            Input = cli:wait_cmd(?PROMPT_TIMEOUT),
            if Input == timeout -> 
                    Decision = {timeout, 0, NumOfBikers};
                Input == behind ->
                    % set speed to solve conflicts
                    Status = biker_repository:get_status(BikerId, Round),
                    {Strategy, _, Player} = Input,
                    Speed = Status#status.speed,
                    Decision = {Strategy, Speed, Player};
                true -> Decision = Input
            end;
        true -> 
            Decision = {game_over, 0, NumOfBikers}
    end,
	{FStrategy, FSpeed, FPlayer} = Decision,
    {_,_,Micro} = os:timestamp(),
    {FStrategy, FSpeed, FPlayer, Micro}.

validate_speed(Energy, Speed) ->
    MaxSpeed = math:sqrt(Energy / 0.12),
%    ?PRINT(MaxSpeed),
    MIN = lists:min([Speed, MaxSpeed]),
%    ?PRINT(MIN),
    MIN.

check_if_energy(BikerId, Round) ->
%    io:format("Your Status (Round ~p, BikerId ~p):~n", [Round, BikerId]),
    {ok, Status} = biker_repository:get_status(BikerId, Round),
%    io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]),
    if Status#status.energy > 0 -> true;
        true -> false
    end.

play(Decision, Round) ->
    ?PRINT(Decision),
    {ok,Biker} = biker_repository:get_status(Decision#decision.bikerId, Round),
    case Decision#decision.strategy of
        myself ->
            Speed = validate_speed(Biker#status.energy, Decision#decision.speed),
            Position = Biker#status.position + Speed,
            Energy = Biker#status.energy - 0.12 * math:pow(Speed, 2),
            NewBiker = status_repository:create_status(Biker#status.id, ?DISTANCE, Energy, Position, Speed),
            NewBiker;
        behind ->
            PlayerBehindId = Decision#decision.player,
            {ok,PlayerBehindDecision} = biker_repository:get_decision(PlayerBehindId, Round),
            {ok,PlayerBehind} = biker_repository:get_status(PlayerBehindId, Round),
            Speed = validate_speed(Biker#status.energy, PlayerBehindDecision#decision.speed),
            Position = PlayerBehind#status.position + Speed,
            Energy = Biker#status.energy - 0.06 * math:pow(Speed, 2),
            NewBiker = status_repository:create_status(Biker#status.id, ?DISTANCE, Energy, Position, Speed),
            NewBiker;
        boost ->
            Speed = 3.87 * math:sqrt(Biker#status.energy),
            Position = Biker#status.position + Speed,
            Energy = 0.0,
            NewBiker = status_repository:create_status(Biker#status.id, ?DISTANCE, Energy, Position, Speed), 
            NewBiker;
        timeout ->  
            Speed = validate_speed(Biker#status.energy, Biker#status.speed),
            Energy = Biker#status.energy - 0.12 * math:pow(Speed, 2), 
            Position = Biker#status.position + Speed,
            NewBiker = status_repository:create_status(Biker#status.id, ?DISTANCE, Energy, Position, Speed), 
            NewBiker;
        game_over ->
            NewBiker = Biker,
            NewBiker
    end.

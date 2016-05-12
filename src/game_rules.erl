-module(game_rules).
-compile(export_all).
-include("biker.hrl").
-include("msgy.hrl").

get_user_decision(BikerId, Round, NumOfBikers) ->
    CanPlay = check_if_can_play(BikerId, Round),
    if CanPlay == true ->
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
    Decision.

validate_speed(Energy, Speed, B) ->
    ?PRINT(Speed),
    MaxSpeed = max_speed(Energy, B),
    ?PRINT(MaxSpeed),
    MIN = lists:min([Speed, MaxSpeed]),
    ?PRINT(MIN),
    MIN.

max_speed(Energy, B) ->
    float(math:sqrt(Energy / B)).

validate_position(Position) ->
    if Position > ?DISTANCE -> ?DISTANCE;
    true -> Position
    end.

check_if_can_play(BikerId, Round) ->
%    io:format("Your Status (Round ~p, BikerId ~p):~n", [Round, BikerId]),
    {ok, Status} = biker_repository:get_status(BikerId, Round),
%    io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]),
    if Status#status.energy > 0, Status#status.position < ?DISTANCE -> true;
        true -> false
    end.

check_if_can_play_2(Position, Energy) -> 
    if Energy > 0, Position < ?DISTANCE -> true;
        true -> false
    end.


check_if_end(Round, NumOfBikers) ->
    Set = [ BikerId || BikerId <- lists:seq(0, NumOfBikers-1), check_if_can_play(BikerId, Round)],
    ?PRINT(Set),
    Set == [].

play(Decision, Round) ->
    ?PRINT(Decision),
    {ok,Biker} = biker_repository:get_status(Decision#decision.bikerId, Round),
    case Decision#decision.strategy of
        myself ->
            Speed = validate_speed(Biker#status.energy, Decision#decision.speed, ?B_MYSELF),
            Position = Biker#status.position + Speed,
            Energy = Biker#status.energy - ?B_MYSELF * math:pow(Speed, 2);
        behind ->
            PlayerBehindId = Decision#decision.player,
            {ok,PlayerBehindDecision} = biker_repository:get_decision(PlayerBehindId, Round),
            {ok,PlayerBehind} = biker_repository:get_status(PlayerBehindId, Round),
            Speed = validate_speed(Biker#status.energy, PlayerBehindDecision#decision.speed, ?B_BEHIND),
            Position = PlayerBehind#status.position + Speed,
            ?PRINT(Speed),
            ?PRINT(PlayerBehindDecision#decision.speed),
            Energy = Biker#status.energy - ?B_BEHIND * math:pow(Speed, 2);
        boost ->
            Speed = ?BOOST * math:sqrt(Biker#status.energy),
            Position = Biker#status.position + Speed,
            Energy = 0.0;
        timeout ->  
            Speed = validate_speed(Biker#status.energy, Biker#status.speed, ?B_MYSELF),
            Energy = Biker#status.energy - ?B_MYSELF * math:pow(Speed, 2), 
            Position = Biker#status.position + Speed;
        game_over -> Speed = Energy = Position = 0
    end,
    
    if Decision#decision.strategy == game_over -> NewBiker = Biker;
        true ->
        CanPlay = check_if_can_play_2(Position, Energy),
        if CanPlay == false -> 
                Stop = Decision#decision.ts,
                Pos = validate_position(Position);
            true -> Stop = Biker#status.rank, Pos = Position
        end,
        NewBiker = status_repository:create_status(Biker#status.id, Stop, Energy, Pos, Speed)
    end,

    NewBiker.

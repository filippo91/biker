-module(biker).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/1,
         put_info/2,
         info_race/2,
         ping/0
        ]).

%% Public API
start_race(BikerId) ->  
    Round = 0,
    InitialStatus = status_repository:create_status(BikerId),
    biker_repository:save_status(BikerId, Round, InitialStatus),
    round_node(BikerId, 0).

put_info(BikerId, Round) ->
    InitialStatus = status_repository:create_status(BikerId),
    biker_repository:save_status(BikerId, Round, InitialStatus). 

info_race(BikerId, Round) ->
    {ok,InitialStatus} = biker_repository:get_status(BikerId, Round),
    io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [InitialStatus#status.distance, InitialStatus#status.energy, InitialStatus#status.position, InitialStatus#status.speed]),
    {ok, Decision} = biker_repository:get_decision(BikerId, Round),
    io:format("Decision:~n Strategy: ~s~nSpeed~f~n",  [Decision#decision.strategy, Decision#decision.speed]).

set_decision(Input, BikerId, Round) -> 
    {Strategy, Speed, Player} = Input,
    Decision = decision_repository:create_decision(Strategy, Player, Speed),
%    io:format("Strategy: ~p~nSpeed: ~p~nPlayer: ~p~n", [Strategy, Speed, Player]),
    biker_repository:save_decision(BikerId, Round, Decision).
%    {ok, X} = biker_repository:get_decision(BikerId, Round),
%    io:format("Strategy: ~p~nSpeed: ~p~nPlayer: ~p~n", [X#decision.strategy, X#decision.speed, X#decision.player]).

round_node(BikerId, ?N_ROUND) ->
    BikerId, ?N_ROUND;

round_node(BikerId, Round) ->
    StillEnergy = check_if_energy(BikerId, Round),
    if StillEnergy == true -> 
            Input = wait_cmd(?PROMPT_TIMEOUT),
            if Input == timeout -> Decision = {timeout, ?DEFAULT_SPEED, ?N_BIKER};
                true -> Decision = Input
            end;
        true -> 
            Decision = {game_over, ?DEFAULT_SPEED, ?N_BIKER}
    end,
    set_decision(Decision, BikerId, Round),
    notify_decision(BikerId, Round),
    wait_for_decisions(0,Round),
    NewStatus = update_status(BikerId, Round),
    biker_repository:save_status(BikerId, Round+1, NewStatus),
    UpdatedView = [update_status(Biker, Round) || Biker <- lists:seq(0, ?N_BIKER-1)],
    show_results(UpdatedView, Round+1),
    round_node(BikerId, Round+1).

show_results(UpdatedView, Round) ->
    SortedUpdatedView = lists:sort(fun(A, B) -> A#status.position > B#status.position end, UpdatedView),
    io:format("State of the race at Round ~p~n",[Round]),
    [ print_state(Biker)  || Biker <- SortedUpdatedView].

print_state(Biker) ->
    io:format("BikerId: ~B~nDistance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [Biker#status.id, Biker#status.distance, Biker#status.energy, Biker#status.position, Biker#status.speed]).

notify_decision(BikerId, Round) ->
    io:format("notify decision ~p~n", [Round]),
    biker_repository:set_notification(BikerId, Round).

wait_for_decisions(?N_BIKER-1, Round) ->
    {OperationState, Not} = biker_repository:get_notification(?N_BIKER-1, Round),
    io:format("wait for ~B Round ~B, operation ~p, ~p~n", [?N_BIKER-1, Round, OperationState, Not]),
    if OperationState == ok, Not == not_found ->
            timer:sleep(10000),
            wait_for_decisions(?N_BIKER-1, Round);
        true ->
            great
    end;

wait_for_decisions(BikerId, Round) ->
    {OperationState, Not} = biker_repository:get_notification(BikerId, Round),
    io:format("wait for ~B Round ~B, operation ~p, ~p~n", [BikerId, Round, OperationState, Not]),
    if OperationState == ok, Not == not_found ->
            timer:sleep(10000),
            wait_for_decisions(BikerId, Round);
        true ->
            great
    end,
    wait_for_decisions(BikerId+1, Round).

update_status(BikerId, Round) -> 
    {ok,Status} = biker_repository:get_status(BikerId, Round),
    {ok,Decision} = biker_repository:get_decision(BikerId, Round),
    case Decision#decision.strategy of
        myself -> 
            %io:format("updating state...~n"),
            Speed = Decision#decision.speed,
            Position = Status#status.position + Speed,
            Energy = Status#status.energy - 0.12 * math:pow(Speed, 2),
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            %io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [NewStatus#status.distance, NewStatus#status.energy, NewStatus#status.position, NewStatus#status.speed]),
            NewStatus;
        behind ->
            PlayerId = Decision#decision.player,
            {ok,PlayerDecision} = biker_repository:get_decision(PlayerId, Round),
            if  PlayerDecision#decision.strategy == behind, 
                PlayerDecision#decision.player == BikerId,
                BikerId > PlayerId ->
                    Speed = ?DEFAULT_SPEED;
                true ->
                    Speed = PlayerDecision#decision.speed
            end,
            Position = Status#status.position + Speed,
            Energy = Status#status.energy - 0.06 * math:pow(Speed, 2),
            
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
                    NewStatus;
                
        boost ->
            Speed = 3.87 * math:sqrt(Status#status.energy),
            Position = Status#status.position + Speed,
            Energy = 0.0,
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            NewStatus;
        timeout -> 
            MaxSpeed = math:sqrt(Status#status.energy / 0.12),
            Speed = lists:min([Status#status.speed, MaxSpeed]),
            Energy = Status#status.energy - 0.12 * math:pow(Speed, 2),
            Position = Status#status.position + Speed,
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            NewStatus;
        game_over -> 
            NewStatus = Status,
            NewStatus
    end.

check_if_energy(BikerId, Round) ->
    io:format("Your Status (Round ~p, BikerId ~p):~n", [Round, BikerId]),
    {ok, Status} = biker_repository:get_status(BikerId, Round),
    io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]),
    if Status#status.energy > 0 -> true;
        true -> false
    end.

user_prompt() ->
    {ok, Strategy} = io:read("Strategy> "),
    io:format("Strategy: ~s~n", [Strategy]),
    case Strategy of
        myself -> 
            {ok, Input} = io:read("Speed>"),
            Speed = float(Input),
            Player = ?N_BIKER;
        behind ->
            {ok, Player} = io:read("Who?> "),
            Speed = ?DEFAULT_SPEED;
        boost ->
            Speed = ?DEFAULT_SPEED,
            Player = ?N_BIKER
    end,
    {Strategy, Speed, Player}.

wait_cmd(Timeout) ->
    Parent = self(),
    Pid = spawn(fun() -> Parent ! user_prompt() end),
    receive
        Data -> Data
    after Timeout ->
        exit(Pid, kill),
        timeout
    end.
 
%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

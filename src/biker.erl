-module(biker).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/1,
         put_info/2,
         info_race/2,
         ping/0,
         update_status/2,
         validate_speed/2
        ]).

%% Public API
start_race(BikerId) -> 
    MasterId = 0,
    Round = 0,
    if BikerId == MasterId ->
        set_initial_state(0, Round),
        notify_master_decision(MasterId, Round),
        master_node(MasterId, Round);
        true -> 
        round_node(MasterId, BikerId, Round)
    end.   

set_initial_state(?N_BIKER, Round) ->
    Round, ?N_BIKER;

set_initial_state(BikerId, Round) ->
    InitialStatus = status_repository:create_status(BikerId),
    biker_repository:save_status(BikerId, Round, InitialStatus),
    set_initial_state(BikerId+1, Round).

put_info(BikerId, Round) ->
    InitialStatus = status_repository:create_status(BikerId),
    biker_repository:save_status(BikerId, Round, InitialStatus). 

info_race(BikerId, Round) ->
    {ok,InitialStatus} = biker_repository:get_status(BikerId, Round),
    io:format("Id: ~p~nDistance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [InitialStatus#status.id, InitialStatus#status.distance, InitialStatus#status.energy, InitialStatus#status.position, InitialStatus#status.speed]),
    {ok, Decision} = biker_repository:get_decision(BikerId, Round),
    io:format("Decision:~n Strategy: ~s~nSpeed~f~n",  [Decision#decision.strategy, Decision#decision.speed]).

set_decision(Input, BikerId, Round) -> 
    {Strategy, Speed, Player} = Input,
    Decision = decision_repository:create_decision(Strategy, Player, Speed),
%    io:format("Strategy: ~p~nSpeed: ~p~nPlayer: ~p~n", [Strategy, Speed, Player]),
    biker_repository:save_decision(BikerId, Round, Decision).
%    {ok, X} = biker_repository:get_decision(BikerId, Round),
%    io:format("Strategy: ~p~nSpeed: ~p~nPlayer: ~p~n", [X#decision.strategy, X#decision.speed, X#decision.player]).

master_node(MasterId, ?N_ROUND) ->
    KVEntries = [biker_repository:get_status(BikerId, ?N_ROUND) || BikerId <- lists:seq(0, ?N_BIKER-1)],
    OldView = [ Status || {_,Status}  <- KVEntries ],
    SortedOldView = lists:sort(
        fun(A, B) -> if A#status.position =< B#status.position, A#status.id > B#status.id -> true;
                    true -> false
                    end
        end, 
        OldView),
    show_results(SortedOldView, ?N_ROUND),
    MasterId, ?N_ROUND;

master_node(MasterId, Round) ->
    KVEntries = [biker_repository:get_status(BikerId, Round) || BikerId <- lists:seq(0, ?N_BIKER-1)],
    OldView = [ Status || {_,Status}  <- KVEntries ],
    SortedOldView = lists:sort(
        fun(A, B) -> if A#status.position =< B#status.position, A#status.id > B#status.id -> true;
                    true -> false
                    end
        end, 
        OldView),
    show_results(SortedOldView, Round),
    StillEnergy = check_if_energy(MasterId, Round),
    if StillEnergy == true -> 
            Input = wait_cmd(?PROMPT_TIMEOUT),
            if Input == timeout -> Decision = {timeout, 0, ?N_BIKER};
                true -> Decision = Input    
            end;
        true -> Decision = {game_over, 0, ?N_BIKER}
    end,
    set_decision(Decision, MasterId, Round),
    notify_decision(MasterId, Round),
    wait_for_decisions(0,Round),
    %set_initial_state(0, Round+1),
    UpdatedStatus = [ update_status(Biker#status.id, Round) ||  Biker <- SortedOldView],
    [biker_repository:save_status(NewStatus#status.id, Round+1, NewStatus) || NewStatus <- UpdatedStatus],
    notify_master_decision(MasterId, Round),
    master_node(MasterId, Round+1).

notify_master_decision(MasterId, Round) ->
    biker_repository:set_master_notification(MasterId, Round).

round_node(MasterId, BikerId, ?N_ROUND) ->
    MasterId, BikerId, ?N_ROUND;

round_node(MasterId, BikerId, Round) ->
    wait_for_master(MasterId, Round),
    KVEntries = [biker_repository:get_status(BikerIdIterator, Round) || BikerIdIterator <- lists:seq(0, ?N_BIKER-1)],
    UpdatedView = [ Status || {_,Status}  <- KVEntries ],
    show_results(UpdatedView, Round),
    StillEnergy = check_if_energy(BikerId, Round),
    if StillEnergy == true -> 
            Input = wait_cmd(?PROMPT_TIMEOUT),
            if Input == timeout -> Decision = {timeout, 0, ?N_BIKER};
                true -> Decision = Input    
            end;
        true -> Decision = {game_over, 0, ?N_BIKER}
    end,
    set_decision(Decision, BikerId, Round),
    notify_decision(BikerId, Round),
    round_node(MasterId, BikerId, Round+1).

show_results(UpdatedView, Round) ->
    SortedUpdatedView = lists:sort(fun(A, B) -> A#status.position > B#status.position end, UpdatedView),
    io:format("~n----------------------------------~n"),
    io:format("| State of the race at Round ~p/~p |~n",[Round, ?N_ROUND]),
    io:format("----------------------------------~n"),
%    ?PRINT(UpdatedView),
%    [ print_state(Biker)  || Biker <- UpdatedView].
    [ print_state(Biker)  || Biker <- SortedUpdatedView].

print_state(Biker) ->
%    ?PRINT(Biker),
    io:format("~n--------------~n"),
    io:format("| BikerId: ~B |~n", [Biker#status.id]),
    io:format("--------------~n"),
    io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n", [Biker#status.distance, Biker#status.energy, Biker#status.position, Biker#status.speed]).

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

wait_for_master(MasterId, Round) ->
    {OperationState, Not} = biker_repository:get_master_notification(MasterId, Round),
    if OperationState == ok, Not == not_found ->
            timer:sleep(10000),
            wait_for_master(MasterId, Round);
        true ->
            great
    end.

update_status(BikerId, Round) -> 
    {ok,Status} = biker_repository:get_status(BikerId, Round),
    {ok,Decision} = biker_repository:get_decision(BikerId, Round),
    case Decision#decision.strategy of
        myself -> 
            %io:format("updating state...~n"),
            Speed = validate_speed(Status#status.energy, Decision#decision.speed),
            Position = Status#status.position + Speed,
            Energy = Status#status.energy - 0.12 * math:pow(Speed, 2),
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            %io:format("Distance: ~f~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n",  [NewStatus#status.distance, NewStatus#status.energy, NewStatus#status.position, NewStatus#status.speed]),
            NewStatus;
        behind ->
            PlayerId = Decision#decision.player,
            {ok,PlayerDecision} = biker_repository:get_decision(PlayerId, Round),
            % if a <- b && b <- a, the lower id wins
            if  PlayerDecision#decision.strategy == behind, 
                PlayerDecision#decision.player == BikerId,
                BikerId > PlayerId ->
                    % lose: try to mantain the previous speed
                    Speed = validate_speed(Status#status.energy, Status#status.speed),
                    Position = Status#status.position + Speed,
                    Energy = Status#status.energy - 0.12 * math:pow(Speed, 2);
                true ->
                    % win: get the speed of the player I chose
                    PlayerStatus = biker_repository:get_status(PlayerId, Round+1),
                    Speed = validate_speed(Status#status.energy, PlayerStatus#status.speed),
                    Position = PlayerStatus#status.position + Speed,
                    Energy = Status#status.energy - 0.06 * math:pow(Speed, 2)
            end,
            
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
                    NewStatus;
                
        boost ->
            Speed = 3.87 * math:sqrt(Status#status.energy),
            Position = Status#status.position + Speed,
            Energy = 0.0,
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            NewStatus;
        timeout -> 
            Speed = validate_speed(Status#status.energy, Status#status.speed),
            Energy = Status#status.energy - 0.12 * math:pow(Speed, 2),
            Position = Status#status.position + Speed,
            NewStatus = status_repository:create_status(BikerId, ?DISTANCE, Energy, Position, Speed),
            NewStatus;
        game_over -> 
            NewStatus = Status,
            NewStatus
    end.

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
            Speed = behind;
        boost ->
            Speed = 0,
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

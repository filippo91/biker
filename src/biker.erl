-module(biker).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/1,
         put_info/2,
         info_race/2,
         round_node/2,
         round_master/2,
         ping/0,
         user_prompt/2
        ]).

%% Public API
start_race(BikerId) ->  
    Round = 0,
    if BikerId =:= 0 ->
        timer:apply_after(?ROUND_DURATION, biker, round_master, [0, Round])
    end,
    InitialStatus = status_repository:create_status(),
    biker_repository:save_status(BikerId, Round, InitialStatus),
    round_node(BikerId, 0).

put_info(BikerId, Round) ->
    InitialStatus = status_repository:create_status(),
    biker_repository:save_status(BikerId, Round, InitialStatus). 

info_race(BikerId, Round) ->
    {ok,InitialStatus} = biker_repository:get_status(BikerId, Round),
    io:format("Distance: ~B~nEnergy: ~f~nPosition: ~B~nSpeed: ~B~n",  [InitialStatus#status.distance, InitialStatus#status.energy, InitialStatus#status.position, InitialStatus#status.speed]),
    {ok, Decision} = biker_repository:get_decision(BikerId, Round),
    io:format("Decision:~n Strategy: ~s~nSpeed~B~n",  [Decision#decision.strategy, Decision#decision.speed]).

round_master(?N_BIKER, ?N_ROUND-1) ->
    NewStatus = update_status(?N_BIKER, ?N_ROUND-1),
    %io:format("Distance: ~B~nEnergy: ~f~nPosition: ~B~nSpeed: ~B~n",  [NewStatus#status.distance, NewStatus#status.energy, NewStatus#status.position, NewStatus#status.speed]), 
    biker_repository:save_status(?N_BIKER, ?N_ROUND, NewStatus);

round_master(?N_BIKER, Round) ->
    NewStatus = update_status(?N_BIKER, Round),
    biker_repository:save_status(?N_BIKER, Round+1, NewStatus),
    timer:apply_after(?ROUND_DURATION, biker, round_master, [0, Round+1]);

round_master(BikerId, Round) ->
    NewStatus = update_status(BikerId, Round),
    biker_repository:save_status(BikerId, Round+1, NewStatus),
    round_master(BikerId+1, Round). 

round_node(BikerId, ?N_ROUND) ->
    %timer:kill_after(?ROUND_DURATION, spawn(biker, user_prompt, [BikerId, ?N_ROUND]));
    %% last round -> print status and exit
    {ok, Status} = biker_repository:get_status(BikerId, ?N_ROUND),
    io:format("[ROUND ~B]Your Final Result:~n", [?N_ROUND]),
    io:format("Distance: ~B~nEnergy: ~f~nPosition: ~B~nSpeed: ~B~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]);

round_node(BikerId, Round) ->
    timer:apply_after(?ROUND_DURATION, biker, round_node, [BikerId, Round+1]),
    timer:kill_after(?ROUND_DURATION, spawn(biker, user_prompt, [BikerId, Round])).

update_status(BikerId, Round) -> 
    {ok,Status} = biker_repository:get_status(BikerId, Round),
    {ok,Decision} = biker_repository:get_decision(BikerId, Round),
    io:format("Strategy: ~p~n", [Decision#decision.strategy]),
    case Decision#decision.strategy of
        myself -> 
            %io:format("updating state...~n"),
            Speed = Decision#decision.speed,
            Position = Status#status.position + Speed * 10,
            Energy = Status#status.energy - 0.12 * math:pow(Speed, 2),
            NewStatus = status_repository:create_status(?DISTANCE, Energy, Position, Speed),
            %io:format("Distance: ~B~nEnergy: ~f~nPosition: ~B~nSpeed: ~B~n",  [NewStatus#status.distance, NewStatus#status.energy, NewStatus#status.position, NewStatus#status.speed]),
            NewStatus;
        behind -> 
            'cane';
        boost ->
            'gatto'
    end.

user_prompt(BikerId, Round) ->
    io:format("Your Status (Round ~p, BikerId ~p):~n", [Round, BikerId]),
    {ok, Status} = biker_repository:get_status(BikerId, Round),
    io:format("Distance: ~B~nEnergy: ~f~nPosition: ~B~nSpeed: ~B~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]),
    {ok, Strategy} = io:read("Strategy> "),
    io:format("Strategy: ~s~n", [Strategy]),
    case Strategy of
        myself -> 
            {ok, Speed} = io:read("Speed>"),
            Player = 100;
        behind ->
            {ok, Player} = io:read("Who?> "),
            Speed = 0;
        boost ->
            Player = Speed = 0
    end,
    Decision = decision_repository:create_decision(Strategy, Player, Speed),
    biker_repository:save_decision(BikerId, Round, Decision).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

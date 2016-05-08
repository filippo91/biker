-module(biker).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/1,
         info_race/2,
         round_master/2,
         ping/0,
         user_prompt/2
        ]).

%% Public API
start_race(BikerId) ->  
    Round = 0,
    if BikerId =:= 0 ->
        timer:apply_after(?ROUND_DURATION, round_master, [0, Round+1])
    end,
    status_repository:create_status(),
    round_node(BikerId, ?N_ROUND).

info_race(BikerId, Round) ->
    biker_repository:get_status(BikerId, Round).
    
round_master(?N_BIKER, ?N_ROUND) ->
    NewStatus = update_status(?N_BIKER, ?N_ROUND),
    biker_repository:save_status(NewStatus);

round_master(?N_BIKER, Round) ->
    NewStatus = update_status(?N_BIKER, Round),
    biker_repository:save_status(NewStatus),
    timer:apply_after(?ROUND_DURATION, biker, round_master, [0, Round+1]);

round_master(BikerId, Round) ->
    NewStatus = update_status(BikerId, Round-1),
    biker_repository:save_status(NewStatus),
    round_master(BikerId+1, Round). 

round_node(BikerId, ?N_ROUND) ->
    timer:kill_after(?ROUND_DURATION-500, spawn(biker, user_prompt, [BikerId, ?N_ROUND]));

round_node(BikerId, Round) ->
    timer:apply_after(?ROUND_DURATION, biker, round_node, [BikerId, Round+1]),
    timer:kill_after(?ROUND_DURATION-500, spawn(biker, user_prompt, [BikerId, Round])).

update_status(BikerId, Round) -> 
    Status = biker_repository:get_status(BikerId, Round),
    Decision = biker_repository:get_decision(BikerId, Round),
    Status, Decision,
    status_repository:create_status(10,10,10,10). 

user_prompt(BikerId, Round) ->
    {_,Status} = biker_repository:get_status(BikerId, Round),
    io:format("Your status:~nDistance: ~B~nEnergy: ~B~nPosition: ~B~nSpeed: ~B~n",  [Status#status.distance, Status#status.energy, Status#status.position, Status#status.speed]),
    Decision = decision_repository:create_decision(),
    {ok, [Strategy]} = io:fread("Strategy> ", "~s"),
    Strategy,
    biker_repository:save_decision(BikerId, Round, Decision).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

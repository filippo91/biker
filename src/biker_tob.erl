-module(biker_tob).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/2,
         ping/0
        ]).

%% Public API
start_race(BikerId, NumOfBikers) -> 
    cli:print_logo(),
    MasterId = 0,
    Round = 0,
    if BikerId == MasterId ->
        tob:create_initial_instance(0, Round, NumOfBikers),
        tob:new_round(MasterId, Round),
        master_node(MasterId, Round, NumOfBikers);
        true -> 
        ordinary_node(MasterId, BikerId, Round, NumOfBikers)
    end.   

master_node(MasterId, Round, NumOfBikers) ->
    TheEnd = game_rules:check_if_end(Round, NumOfBikers),
    if TheEnd == false ->
        play_game(MasterId,Round, NumOfBikers),
        Decisions = tob:receive_decisions(0, Round, NumOfBikers, []),
        ?PRINT(Decisions),
        SortedDecisions = tob:decide_order(Decisions, Round),
        ?PRINT(SortedDecisions),
        UpdatedStatus = [ game_rules:play(Decision, Round) ||  Decision <- SortedDecisions],
        tob:broadcast_decision(UpdatedStatus, Round+1),
        tob:new_round(MasterId, Round+1),
        master_node(MasterId, Round+1, NumOfBikers);
     true -> 
        cli:show_ranking(Round, NumOfBikers),
        cli:show_winner(Round, NumOfBikers)
     end.

ordinary_node(MasterId, BikerId, Round, NumOfBikers) ->
    wait_for_master(MasterId, Round),
    TheEnd = game_rules:check_if_end(Round, NumOfBikers),
    if TheEnd == false ->
        play_game(BikerId,Round, NumOfBikers),
        ordinary_node(MasterId, BikerId, Round+1, NumOfBikers);
    true -> 
        cli:show_ranking(Round, NumOfBikers),
        cli:show_winner(Round, NumOfBikers)
    end.

play_game(BikerId,Round, NumOfBikers) ->
    cli:show_ranking(Round, NumOfBikers),
    {ok, Me} = biker_repository:get_status(BikerId, Round),                 
    cli:show_biker_info(Me),
    Input = game_rules:get_user_decision(BikerId, Round, NumOfBikers),
    set_decision(Input, BikerId, Round).
    

set_decision(Input, BikerId, Round) -> 
    {Strategy, Speed, Player} = Input,
    Decision = decision_tob_repository:create_decision(BikerId, Strategy, Player, Speed, 0),
    biker_repository:save_decision(BikerId, Round, Decision).

wait_for_master(MasterId, Round) ->
    {OperationState, Not} = biker_repository:get_master_notification(MasterId, Round), 
    if OperationState == ok, Not == not_found ->
            timer:sleep(1000),
            wait_for_master(MasterId, Round);
        true ->
            great
    end.

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

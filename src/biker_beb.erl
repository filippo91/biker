-module(biker_beb).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/2,
         ping/0
        ]).

%% Public API
start_race(BikerId, NumOfBikers) -> 
    Round = 0,
    create_initial_status(BikerId, Round),
    node(BikerId, Round, NumOfBikers).

node(BikerId, Round, NumOfBikers) ->
    beb:receive_decisions(BikerId, Round, NumOfBikers, []),
    cli:show_previous_round(Round, NumOfBikers),
    TheEnd = game_rules:check_if_end(Round, NumOfBikers),
    if TheEnd == false -> 
        Input = game_rules:get_user_decision(BikerId, Round, NumOfBikers),
        Decision = create_decision(Input, BikerId),
        NewStatus = game_rules:play(Decision, Round),
        beb:broadcast_decision(NewStatus, Round+1),
        node(BikerId, Round+1, NumOfBikers);
        true -> theEnd
    end.

create_initial_status(BikerId, Round) ->
    InitialStatus = status_repository:create_status(BikerId),
    beb:broadcast_decision(InitialStatus, Round).

create_decision(Input, BikerId) -> 
    {Strategy, Speed, Player, TS} = Input,
    Decision = decision_repository:create_decision(BikerId, Strategy, Player, Speed, TS),
    Decision.

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

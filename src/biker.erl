-module(biker).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("msgy.hrl").

-export([
         start_race/2,
         info_race/2,
         ping/0
        ]).

%% Public API
start_race(BikerId, N) ->  
    Round = 0,
    if BikerId =:= 0 ->
        round_master(BikerId, Round, N)
    end,
    round_node(BikerId, Round).

info_race(BikerId, Round) ->
    biker_repository:get_status(BikerId, Round).
    
round_master(BikerId, Round, N) ->
    N,
    Status = status_repository:create_status(),
    biker_repository:save_status(BikerId, Round, Status).

round_node(BikerId, Round) ->
    BikerId, 
    Round.

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

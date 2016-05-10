-module(biker_repository).
-export([
        save_status/3, 
        save_decision/3, 
        get_status/2,
        get_decision/2,
        get_notification/2,
        set_notification/2,
        get_master_notification/2,
        set_master_notification/2
        ]).

save_status(BikerId, Round, Status) ->
    BikerKey=generate_key(BikerId, Round, "status"),
    kvstore:put(BikerKey, Status). 

save_decision(BikerId, Round, Decision) ->
    BikerKey=generate_key(BikerId, Round, "decision"),
    kvstore:put(BikerKey, Decision). 

get_status(BikerId, Round) ->
    BikerKey=generate_key(BikerId, Round, "status"),
    kvstore:get(BikerKey).

get_decision(BikerId, Round) ->
    BikerKey=generate_key(BikerId, Round, "decision"),
    kvstore:get(BikerKey).

get_notification(BikerId, Round) ->
    BikerKey=generate_key(BikerId, Round, "ready"),
    kvstore:get(BikerKey).

set_notification(BikerId, Round) ->
    BikerKey=generate_key(BikerId, Round, "ready"),
    kvstore:put(BikerKey, Round).

get_master_notification(MasterId, Round) ->
    Key=generate_key(MasterId, Round, "masterNot"),
    kvstore:get(Key).
    
set_master_notification(MasterId, Round) ->
    Key=generate_key(MasterId, Round, "masterNot"),
    kvstore:put(Key, Round).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
generate_key(BikerId, Round, Type) ->
    [BikerId, Type, Round]. 

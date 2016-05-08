-module(biker_repository).
-export([
        save_status/3, 
        save_decision/3, 
        get_status/2,
        get_decision/2
        ]).

save_status(BikerId, Round, Status) ->
    BikerKey=generate_key(BikerId, Round, "status"),
    kvstore:put(BikerKey, Status). 

save_decision(BikerId, Round, Decision) ->
    BikerKey=generate_key(BikerId, Round, "decision"),
    kvstore:put(BikerKey, Decision). 

get_status(BikerId, Round)->
    BikerKey=generate_key(BikerId, Round, "status"),
    kvstore:get(BikerKey).

get_decision(BikerId, Round)->
    BikerKey=generate_key(BikerId, Round, "decision"),
    kvstore:get(BikerKey).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
generate_key(BikerId, Round, Type) ->
    [BikerId, Type, Round]. 

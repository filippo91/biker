-module(biker_repository).
-export([
        save_status/3, 
        get_status/2
        ]).

save_status(BikerId, Round, Status) ->
    BikerKey=generate_key(BikerId, Round),
    kvstore:put(BikerKey, Status). 

get_status(BikerId, Round)->
    BikerKey=generate_key(BikerId, Round),
    kvstore:get(BikerKey).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
generate_key(BikerId, Round) ->
    [BikerId, "_status_",Round]. 

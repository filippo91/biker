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
    cli:print_logo(),
    Round = 0,
    create_initial_instance(BikerId, Round, NumOfBikers),
    node(BikerId, Round, NumOfBikers).

node(BikerId, Round, NumOfBikers) ->
    TheEnd = game_rules_beb:check_if_end(BikerId, Round, NumOfBikers),
    if TheEnd == false -> 
        cli:show_ranking_beb(BikerId, Round, NumOfBikers),
        {ok, Me} = biker_repository:get_status_beb(BikerId, BikerId, Round),           
        cli:show_biker_info(Me),
        Input = game_rules_beb:get_user_decision(BikerId, BikerId, Round, NumOfBikers),
        Decision = create_decision(Input, BikerId),
        beb:broadcast_decision(Decision, Round),
        Decisions = beb:receive_decisions(0, Round, NumOfBikers, []), 
        ?PRINT(Decisions),
		OrderedDecisions = decide_order(Decisions, Round, BikerId),
        ?PRINT(OrderedDecisions),
        UpdatedStatus = [ game_rules_beb:play(BikerId, Dec, Round) || Dec <- OrderedDecisions],
        ?PRINT(UpdatedStatus),
        [biker_repository:save_status_beb(BikerId, NewStatus#status.id, Round+1, NewStatus) || NewStatus <- UpdatedStatus],
        node(BikerId, Round+1, NumOfBikers);
        true -> 
            cli:show_ranking_beb(BikerId, Round, NumOfBikers),
            cli:show_winner_beb(BikerId, Round, NumOfBikers)
    end.

create_initial_instance(BikerId, Round, NumOfBikers) ->
    Instance = [status_repository:create_status(CurrBikerId) || CurrBikerId <- lists:seq(0, NumOfBikers-1)],
    [biker_repository:save_status_beb(BikerId, Status#status.id, Round, Status) || Status <- Instance].

create_decision(Input, BikerId) -> 
    {Strategy, Speed, Player} = Input,
    Decision = decision_repository:create_decision(BikerId, Strategy, Player, Speed, 0),
    Decision.

decide_order(Decisions, Round, OwnerId) ->
    SortedDecisions = sort(Decisions),
    Solved = [solve_conflicts(Decision, Round, OwnerId) || Decision <- SortedDecisions],
    Solved.

sort(Decisions) ->
    lists:sort(
        fun(A, B) ->  
            if A#decision.ts =< B#decision.ts -> true;
                true -> false
            end
         end,
         Decisions).

solve_conflicts(Decision, Round, OwnerId) ->
    ?PRINT(Decision),
    BikerId = Decision#decision.bikerId,
    Biker = biker_repository:get_status_beb(OwnerId, BikerId, Round),
    PlayerBehindId = Decision#decision.player,
    {ok,PlayerBehindDecision} = biker_repository:get_decision(PlayerBehindId, Round), 
    if  PlayerBehindDecision#decision.strategy == behind,
        PlayerBehindDecision#decision.player == Biker#status.id,
        Decision#decision.ts > PlayerBehindDecision#decision.ts ->
        % there is a conflict
        % lose: try to mantain the previous speed
            Speed = game_rules_beb:validate_speed(Biker#status.energy, Biker#status.speed),
            NewDecision = decision_tob_repository:create_decision(BikerId, myself, 0, Speed, Decision#decision.ts),
            NewDecision;
        true ->
            % win: keep the decision
            Decision
    end.


%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).



-module(decision_tob_repository).
-export([create_decision/0, create_decision/5]).
-include("msgy_tob.hrl").

create_decision() ->
    #decision{strategy=myself,
             player=0,
             speed=1}.

create_decision(BikerId, Strategy, Player, Speed, TS) ->
    #decision{bikerId=BikerId,
             strategy=Strategy,
             player=Player,
             speed=Speed,
             ts=TS}.

-module(decision_repository).
-export([create_decision/0, create_decision/3]).
-include("msgy.hrl").

create_decision() ->
    #decision{strategy=myself,
             player=0,
             speed=1}.

create_decision(Strategy, Player, Speed) ->
    #decision{strategy=Strategy,
             player=Player,
             speed=Speed}.

-module(status_repository).
-export([create_status/1, create_status/5]).
-include("msgy.hrl").

create_status(BikerId) ->
    #status{id=BikerId,
            rank=0,
             energy=112.0,
             position=0.0,
             speed=0.0}.

create_status(BikerId, Rank, Energy, Position, Speed) ->
    #status{id=BikerId,
            rank=Rank,
             energy=Energy,
             position=Position,
             speed=Speed}.

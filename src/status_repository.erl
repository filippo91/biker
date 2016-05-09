-module(status_repository).
-export([create_status/1, create_status/5]).
-include("msgy.hrl").

create_status(BikerId) ->
    #status{id=BikerId,
            distance=100.0,
             energy=112.0,
             position=0.0,
             speed=0.0}.

create_status(BikerId, Distance, Energy, Position, Speed) ->
    #status{id=BikerId,
            distance=Distance,
             energy=Energy,
             position=Position,
             speed=Speed}.

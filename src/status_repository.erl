-module(status_repository).
-export([create_status/0, create_status/4]).
-include("msgy.hrl").

create_status() ->
    #status{distance=100.0,
             energy=112.0,
             position=0.0,
             speed=0.0}.

create_status(Distance, Energy, Position, Speed) ->
    #status{distance=Distance,
             energy=Energy,
             position=Position,
             speed=Speed}.

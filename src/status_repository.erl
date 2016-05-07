-module(status_repository).
-export([create_status/0, create_status/4]).
-include("msgy.hrl").

create_status() ->
    #status{distance=100,
             energy=112,
             position=0,
             speed=0}.

create_status(Distance, Energy, Position, Speed) ->
    #status{distance=Distance,
             energy=Energy,
             position=Position,
             speed=Speed}.

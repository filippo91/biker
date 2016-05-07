-module(biker).
-compile({no_auto_import,[get/1]}).
-compile({no_auto_import,[put/2]}).
-import(kvstore, [get/1, put/2]).
-include("biker.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         start_race/2,
         stop_race/1,
         ping/0
        ]).

%% Public API
start_race(Key, Value) ->  
    put(Key, Value).

stop_race(Key) ->
    get(Key).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, biker),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, biker_vnode_master).

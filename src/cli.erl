-module(cli).
-compile(export_all).
-include("msgy.hrl").
-include("biker.hrl").

show_previous_round(Round, NumOfBikers) ->
    % Get previous Round information
    KVEntries = [biker_repository:get_status(BikerId, Round) || BikerId <- lists:seq(0, NumOfBikers-1)],
    OldView = [ Status || {_,Status}  <- KVEntries ],
    ?PRINT(OldView),
	% Sort in descending order considering players' position
    SortedOldView = lists:sort(
        fun(A, B) -> if A#status.position =< B#status.position, A#status.id > B#status.id -> true;
                    true -> false
                    end
        end,
        OldView),
	% display the rank
	show_results(SortedOldView, Round),
	SortedOldView.

show_results(UpdatedView, Round) ->
    SortedUpdatedView = lists:sort(fun(A, B) -> A#status.position > B#status.position end, UpdatedView),
    io:format("~n--------------------------------~n"),
    io:format("| State of the race at Round ~p |~n",[Round]),
    io:format("--------------------------------~n"),
    ?PRINT(UpdatedView),
%    [ print_state(Biker)  || Biker <- UpdatedView].
    [ print_state(Biker)  || Biker <- SortedUpdatedView].

print_state(Biker) ->
    ?PRINT(Biker),
    io:format("~n--------------~n"),
    io:format("| BikerId: ~B |~n", [Biker#status.id]),
    io:format("--------------~n"),
    io:format("Rank: ~B~nEnergy: ~f~nPosition: ~f~nSpeed: ~f~n", [Biker#status.rank, Biker#status.energy, Biker#status.position, Biker#status.speed]).

% Get input from the user according to the business rules.
% Add a timestamp to record conflicts.
user_prompt() ->
    {ok, Strategy} = io:read("Strategy> "),
    io:format("Strategy: ~s~n", [Strategy]),
    case Strategy of
        myself ->
            {ok, Input} = io:read("Speed>"),
            Speed = float(Input),
            Player = 0;
        behind ->
            {ok, Player} = io:read("Who?> "),
            Speed = behind;
        boost ->
            Speed = 0,
            Player = 0 
    end,
%    {_,_,Micro} = os:timestamp(),
%    {Strategy, Speed, Player, Micro}.
    {Strategy, Speed, Player}.

% Implements timed input.
% It shuts down the user prompt if the time is over.
wait_cmd(Timeout) ->
    Parent = self(),
    Pid = spawn(fun() -> Parent ! user_prompt() end),
    receive
        Data -> Data
    after Timeout ->
        exit(Pid, kill),
        timeout
    end.


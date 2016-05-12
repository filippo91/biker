-module(cli).
-compile(export_all).
-include("msgy.hrl").
-include("biker.hrl").

print_logo() ->
    io:format("
______ _ _         ______               
| ___ (_) |        | ___ \\              
| |_/ /_| | _____  | |_/ /__ _  ___ ___ 
| ___ \\ | |/ / _\\ |    // _` |/ __/ _ \
| |_/ / |   <  __/ | |\\ \\ (_| | (_|  __/
\\____/|_|_|\\_\\___| \\_| \\_\\__,_|\\___\\___|
                                        
    ").


show_previous_round(Round, NumOfBikers) ->
	SortedOldView = get_ordered_status(Round, NumOfBikers),
    show_results(SortedOldView, Round),
	SortedOldView.

get_ordered_status_beb(OwnerId, Round, NumOfBikers) ->
    % Get previous Round information
    KVEntries = [biker_repository:get_status_beb(OwnerId, BikerId, Round) || BikerId <- lists:seq(0, NumOfBikers-1)],
    
    OldView = [ Status || {_,Status}  <- KVEntries ],
    ?PRINT(OldView),
	% Sort in descending order considering players' position
    SortedOldView = lists:sort(
        fun(A, B) -> 
            if A#status.position < B#status.position -> false;
            A#status.position == B#status.position ->
                if A#status.rank == 0, B#status.rank == 0 -> false;
                A#status.rank =/= 0, B#status.rank =/= 0 -> A#status.rank < B#status.rank;
                true -> true
                end;
            true -> true
            end
        end,
        OldView),
    SortedOldView.

get_ordered_status(Round, NumOfBikers) ->
    % Get previous Round information
    KVEntries = [biker_repository:get_status(BikerId, Round) || BikerId <- lists:seq(0, NumOfBikers-1)],
    OldView = [ Status || {_,Status}  <- KVEntries ],
    ?PRINT(OldView),
	% Sort in descending order considering players' position
    SortedOldView = lists:sort(
        fun(A, B) -> 
            if A#status.position < B#status.position -> false;
            A#status.position == B#status.position ->
                if A#status.rank == 0, B#status.rank == 0 -> false;
                A#status.rank =/= 0, B#status.rank =/= 0 -> A#status.rank < B#status.rank;
                true -> true
                end;
            true -> true
            end
        end,
        OldView),
    SortedOldView.

show_biker_info(Biker) ->
     MaxSpeed = game_rules:max_speed(Biker#status.energy, ?B_MYSELF),
     io:format("   __@  ~n"),
     io:format("  _`\\<,_  Info for biker: #~B~n", [Biker#status.id]),
	 io:format("(*)/ (*)  "),
     io:format("Energy: ~.1f~n", [Biker#status.energy]),
	 io:format("~~~~~~~~~~~~~~~~~~ Max speed: ~.1f~n", [MaxSpeed]),
    io:format("          Commands: myself, behind, boost~n~n").
show_results(View, Round) ->
    %SortedUpdatedView = lists:sort(fun(A, B) -> A#status.position > B#status.position end, UpdatedView),
    io:format("~n--------------------------------~n"),
    io:format("| State of the race at Round ~p |~n",[Round]),
    io:format("--------------------------------~n"),
    ?PRINT(View),
%    [ print_state(Biker)  || Biker <- UpdatedView].
    io:format("
+-----------------------+
|        Round ~B        |
+-----------------------+
| Contestant | Position |
+------------+----------+
", [Round]),
    [ print_rank(Biker)  || Biker <- View].

print_rank(Biker) -> 
io:format(
"| Biker ~B    | ~.1f    |
+------------+----------+
", [Biker#status.id, Biker#status.position]).


show_winner_beb(OwnerId, Round, NumOfBikers) ->
    View = get_ordered_status_beb(OwnerId, Round, NumOfBikers),
    Winner = hd(View),
	io:format("
	      .-=========-.
              \\'-=======-'/   
              _|   .=.   |_          Biker #~B
             ((|  {{1}}  |))   	  Congratulations!
              \\|   /|\\   |/      you are the winner
               \\__ '`' __/
                 _`) (`_
               _/_______\\_
              /___________\\
    ", [Winner#status.id]).

show_ranking_beb(OwnerId, Round, NumOfBikers) ->
    View = get_ordered_status_beb(OwnerId, Round, NumOfBikers),
    ?PRINT(View),
io:format(
"
+---------------------------------------------------+
|                  Rank Round #~2..0B                   |
+---------------------------------------------------+
| # | Contestant | Position |   Last Move  | Behind |
+---+------------+----------+--------------+--------+
", [Round]),
    lists:foreach(fun(X)-> print_complete_info(lists:nth(X, View), X, Round) end,
        lists:seq(1, NumOfBikers)).
        
%    [ print_complete_info(Biker, X, Round)  || {Biker, X} <- View, lists:seq(1, NumOfBikers)].
    
print_complete_info(Biker, Pos, Round) ->
    ?PRINT(Biker),
    if Round == 0->
        Decision = decision_repository:create_decision(0, nada, 0, 0, 0);
    true -> 
        {ok, Decision} = biker_repository:get_decision(Biker#status.id, Round-1)
    end,

    ?PRINT(Decision),
	TS = Decision#decision.ts,

    if TS == 0 -> {{_, _, _},{Hour, Min, Sec}} = calendar:local_time();
    true -> 
        {{_, _, _},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(TS)
    end,

    io:format(
"| ~B | Biker ~B    | ~8.1f |   ~2..0b:~2..0b:~2..0b   |",
[Pos, Biker#status.id, Biker#status.position, Hour, Min, Sec]),

if Decision#decision.strategy == behind ->
        io:format(" ~B |", [Decision#decision.player]);
    true ->
        io:format("   //   |")
    end,
io:format(
"
+---+------------+----------+--------------+--------+
").

show_winner(Round, NumOfBikers) ->
    View = get_ordered_status(Round, NumOfBikers),
    Winner = hd(View),
	io:format("
	      .-=========-.
              \\'-=======-'/   
              _|   .=.   |_          Biker #~B
             ((|  {{1}}  |))   	  Congratulations!
              \\|   /|\\   |/      you are the winner
               \\__ '`' __/
                 _`) (`_
               _/_______\\_
              /___________\\
    ", [Winner#status.id]).

show_ranking(Round, NumOfBikers) ->
    View = get_ordered_status(Round, NumOfBikers),
    ?PRINT(View),
io:format(
"
+---------------------------------------------------+
|                  Rank Round #~2..0B                   |
+---------------------------------------------------+
| # | Contestant | Position |   Last Move  | Behind |
+---+------------+----------+--------------+--------+
", [Round]),
    lists:foreach(fun(X)-> print_complete_info(lists:nth(X, View), X, Round) end,
        lists:seq(1, NumOfBikers)).
        
%    [ print_complete_info(Biker, X, Round)  || {Biker, X} <- View, lists:seq(1, NumOfBikers)].
    
    
% Get input from the user according to the business rules.
% Add a timestamp to record conflicts.
user_prompt() ->
    {ok, Strategy} = io:read("Your Strategy $ "),
    case Strategy of
        myself ->
            {ok, Input} = io:read("Speed $ "),
            Speed = float(Input),
            Player = 0;
        behind ->
            {ok, Player} = io:read("Who?(biker id) $ "),
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


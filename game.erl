-module(game).
-export([start/1, play_games/0]).

-define(TABLE, player_info).

start(PlayerFile) ->
    {ok, RawPlayerInfo} = file:consult(PlayerFile),
    MasterPID = self(),
    ets:new(?TABLE, [set, named_table, public]),
    PlayerInfo = lists:map(
        fun({Name, Credits}) ->
            {spawn(fun() -> player:start(Name, Credits, MasterPID, RawPlayerInfo) end), Name, Credits, 0}
        end,
        RawPlayerInfo
    ),
    ets:insert(?TABLE, PlayerInfo),
    timer:sleep(200),
    io:format("** Rock, Paper Scissors World Championship **~n~n"),
    io:format("Starting game log...~n~n"),
    play_games(),
    init_move(1).

play_games() ->
    PlayerInfo = ets:tab2list(?TABLE),
    lists:foreach(
        fun({PID, _, Credit, _}) ->
            lists:foreach(
                fun({OpponentPID, _, OpponentCredit, _}) ->
                    if PID =/= OpponentPID, Credit > 0, OpponentCredit > 0 ->
                        PID ! {game_request, Credit, OpponentCredit, OpponentPID};
                    true -> 
                        ok
                    end
                end,
                PlayerInfo
            )
        end,
        PlayerInfo
    ).

init_move(Game_ID) ->
    receive
        {game_tie_move, [Player1_PID, Player1_Move], [Player2_PID, Player2_Move], LogString} ->
            PlayerInfo = ets:tab2list(?TABLE),
            
            {Player1_Name, Player1_Credit, Player2_Name, Player2_Credit} = find_credits(Player1_PID, Player2_PID, PlayerInfo),
            
            if Player1_Credit > 0, Player2_Credit > 0 ->
                case game_result(Player1_Move, Player2_Move) of
                    win ->
                        NewPlayer1Credit = Player1_Credit + 1,
                        NewPlayer2Credit = Player2_Credit - 1,
                        io:format("~p~p:~p -> ~p:~p = ~p loses [~p credits left]~n", [LogString, Player1_Name, Player1_Move, Player2_Name, Player2_Move, Player2_Name, NewPlayer2Credit]),
                        update_player_info(Player1_PID, NewPlayer1Credit, Player2_PID, NewPlayer2Credit),
                        Player1_PID ! {game_finished, Player1_Name, NewPlayer1Credit},
                        Player2_PID ! {game_finished, Player2_Name, NewPlayer2Credit};
                    lose ->
                        NewPlayer1Credit = Player1_Credit - 1,
                        NewPlayer2Credit = Player2_Credit + 1,
                        io:format("~p~p:~p -> ~p:~p = ~p loses [~p credits left]~n", [LogString, Player1_Name, Player1_Move, Player2_Name, Player2_Move, Player1_Name, NewPlayer1Credit]),
                        update_player_info(Player1_PID, NewPlayer1Credit, Player2_PID, NewPlayer2Credit),
                        Player1_PID ! {game_finished, Player1_Name, NewPlayer1Credit},
                        Player2_PID ! {game_finished, Player2_Name, NewPlayer2Credit};
                    tie ->
                        NewLogString = LogString ++ ", " ++ atom_to_list(Player1_Name) ++ ":" ++ Player1_Move ++ " -> " ++ atom_to_list(Player2_Name) ++ ":" ++ Player2_Move ++ ", ",
                        Player1_PID ! {game_tie, Player2_PID, NewLogString}
                end,
                check_winner(Game_ID + 1),
                init_move(Game_ID + 1);
            true ->
                check_winner(Game_ID),
                init_move(Game_ID)
            end;
        {move, [Player1_PID, Player1_Move], [Player2_PID, Player2_Move]} ->
            PlayerInfo = ets:tab2list(?TABLE),
            
            {Player1_Name, Player1_Credit, Player2_Name, Player2_Credit} = find_credits(Player1_PID, Player2_PID, PlayerInfo),
            
            if Player1_Credit > 0, Player2_Credit > 0 ->
                io:format("+ [~p] new game for ~p -> ~p ~n", [Game_ID, Player1_Name, Player2_Name]),
                case game_result(Player1_Move, Player2_Move) of
                    win ->
                        NewPlayer1Credit = Player1_Credit + 1,
                        NewPlayer2Credit = Player2_Credit - 1,
                        io:format("$ (~p) ~p:~p -> ~p:~p = ~p loses [~p credits left]~n", [Game_ID, Player1_Name, Player1_Move, Player2_Name, Player2_Move, Player2_Name, NewPlayer2Credit]),
                        update_player_info(Player1_PID, NewPlayer1Credit, Player2_PID, NewPlayer2Credit),
                        Player1_PID ! {game_finished, Player1_Name, NewPlayer1Credit},
                        Player2_PID ! {game_finished, Player2_Name, NewPlayer2Credit};
                    lose ->
                        NewPlayer1Credit = Player1_Credit - 1,
                        NewPlayer2Credit = Player2_Credit + 1,
                        io:format("$ (~p) ~p:~p -> ~p:~p = ~p loses [~p credits left]~n", [Game_ID, Player1_Name, Player1_Move, Player2_Name, Player2_Move, Player1_Name, NewPlayer1Credit]),
                        update_player_info(Player1_PID, NewPlayer1Credit, Player2_PID, NewPlayer2Credit),
                        Player1_PID ! {game_finished, Player1_Name, NewPlayer1Credit},
                        Player2_PID ! {game_finished, Player2_Name, NewPlayer2Credit};
                    tie ->
                        LogString = "$ (" ++ integer_to_list(Game_ID) ++ ") " ++ atom_to_list(Player1_Name) ++ ":" ++ Player1_Move ++ " -> " ++ atom_to_list(Player2_Name) ++ ":" ++ Player2_Move ++ ", ",
                        Player1_PID ! {game_tie, Player2_PID, LogString}
                end,
                check_winner(Game_ID + 1),
                init_move(Game_ID + 1);
            true ->
                check_winner(Game_ID),
                init_move(Game_ID)
            end
    end.
    

update_player_info(Player1_PID, NewPlayer1Credit, Player2_PID, NewPlayer2Credit) ->
    PlayerInfo = ets:tab2list(?TABLE),
    UpdatedPlayerInfo = lists:map(
        fun({PID, Name, Credit, UsedCredit}) ->
            case PID of
                Player1_PID -> 
                    {PID, Name, NewPlayer1Credit, UsedCredit + 1};
                Player2_PID -> 
                    {PID, Name, NewPlayer2Credit, UsedCredit + 1};
                _ -> 
                    {PID, Name, Credit, UsedCredit}
            end
        end,
        PlayerInfo
    ),
    ets:delete_all_objects(?TABLE),
    ets:insert(?TABLE, UpdatedPlayerInfo).

check_winner(Game_ID) ->
    PlayerInfo = ets:tab2list(?TABLE),
    {Count, Name} = lists:foldl(
        fun({_PID, Name, Credit, _}, {Count, WinnerName}) ->
            if Credit > 0 ->
                {Count + 1, Name};
            true ->
                {Count, WinnerName}
            end
        end, 
        {0, ""}, PlayerInfo),
    if Count == 1 ->
        io:format("~nWe have a winner...~n~n"),
        print_tournament_report(Game_ID, Name),
        erlang:halt(0);
    true ->
        ok
    end.

print_tournament_report(Game_ID, WinnerName) ->
    io:format("** Tournament Report **~n~n"),
    io:format("Players~n"),
    PlayerInfo = ets:tab2list(?TABLE),
    lists:foreach(
        fun({_PID, Name, Credit, UsedCredit}) ->
            io:format("~p: credits used: ~p, credits remaining: ~p~n", [Name, UsedCredit, Credit])
        end,
        PlayerInfo
    ),
    io:format("----~n"),
    io:format("Total games: ~p~n~n", [Game_ID]),
    io:format("winner: ~p~n~n", [WinnerName]),
    io:format("See you next year...~n~n").

find_credits(Player1_PID, Player2_PID, PlayerInfo) ->
    lists:foldl(
        fun({PID, Name, Credit, _}, {P1_Name, P1_Credit, P2_Name, P2_Credit}) ->
            case PID of
                Player1_PID -> {Name, Credit, P2_Name, P2_Credit};
                Player2_PID -> {P1_Name, P1_Credit, Name, Credit};
                _ -> {P1_Name, P1_Credit, P2_Name, P2_Credit}
            end
        end,
        {"", 0, "", 0},
        PlayerInfo
    ).

game_result("rock", "scissors") -> win;
game_result("rock", "paper") -> lose;
game_result("rock", "rock") -> tie;
game_result("paper", "rock") -> win;
game_result("paper", "scissors") -> lose;
game_result("paper", "paper") -> tie;
game_result("scissors", "paper") -> win;
game_result("scissors", "rock") -> lose;
game_result("scissors", "scissors") -> tie.
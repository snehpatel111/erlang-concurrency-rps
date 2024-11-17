-module(player).
-export([start/4]).

start(Name, Credits, MasterPID, _) ->
    register(Name, self()),
    game_loop(Name, Credits, self(), MasterPID).

game_loop(Name, Credits, PID, MasterPID) ->
    receive
        {game_request, PlayerCredit, OpponentCredits, OpponentPID} ->
            if PlayerCredit > 0, OpponentCredits > 0 ->
                Moves = ["rock", "paper", "scissors"],
                PlayerMove = lists:nth(rand:uniform(length(Moves)), Moves),
                timer:sleep(rand:uniform(90) + 10),
                OpponentPID ! {start_game, PID, MasterPID, PlayerMove},
                game_loop(Name, Credits, PID, MasterPID);
            true ->
                game_loop(Name, Credits, PID, MasterPID)
            end;
        {start_game, OpponentPID, MasterPID, OpponentMove} ->
            if Credits > 0 ->
                Moves = ["rock", "paper", "scissors"],
                PlayerMove = lists:nth(rand:uniform(length(Moves)), Moves),
                MasterPID ! {move, [PID, PlayerMove], [OpponentPID, OpponentMove]},
                game_loop(Name, Credits, PID, MasterPID);
            true ->
                game_loop(Name, Credits, PID, MasterPID)
            end;
        {game_tie, OpponentPID, LogString} ->
            Moves = ["rock", "paper", "scissors"],
            PlayerMove = lists:nth(rand:uniform(length(Moves)), Moves),
            OpponentPID ! {game_tie_resend, PlayerMove, PID, LogString},
            game_loop(Name, Credits, PID, MasterPID);
        {game_tie_resend, OpponentMove, OpponentPID, LogString} ->
            Moves = ["rock", "paper", "scissors"],
            PlayerMove = lists:nth(rand:uniform(length(Moves)), Moves),
            MasterPID ! {game_tie_move, [PID, PlayerMove], [OpponentPID, OpponentMove], LogString},
            game_loop(Name, Credits, PID, MasterPID);
        {game_finished, CurrentPlayerName, CurrentPlayerCredits} ->
            if CurrentPlayerCredits > 0 ->
                game:play_games(),
                game_loop(CurrentPlayerName, CurrentPlayerCredits, PID, MasterPID);
            true ->
                ok
            end
    end.
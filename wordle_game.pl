:- module(wordle_game, [start_game/0]).

:- use_module(utils).
:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Game Loop
% start_game/0:          starts the game by getting a random word and launching the game loop

% game_loop/3:           checks if user guessed the word; if not, updates feedback and repeats
%                        expects word to guess, current user input, and feedback 
start_game :-
    get_random_word(Word),
    game_ui(UserWord),
    game_loop(Word, UserWord, []).

game_loop(Word, Word, OverallFeedback) :- 
    finished_game(Word, OverallFeedback).

game_loop(Word, UserWord, OverallFeedback) :- 
    handle_word(Word, UserWord, 0, NewFeedback),
    append(OverallFeedback, [[UserWord, NewFeedback]], NewOverallFeedback),
    update_game_ui(NewOverallFeedback, NextWord),
    game_loop(Word, NextWord, NewOverallFeedback).

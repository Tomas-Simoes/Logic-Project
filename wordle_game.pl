:- module(wordle_game, [start_game/0]).

:- use_module(utils).
:- use_module(ui).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Game Loop
% start_game/0:          starts the game by getting a random word and launching the game loop

% game_loop/3:           checks if user guessed the word; if not, updates feedback and repeats
%                        expects word to guess, current user input, and feedback 

% handle_user_input/4:   builds feedback list for the input word 
%                        expects target word, user word, current index, resulting feedback

% handle_letter/4:       finds index of letter if it exists in the word
%                        returns -1 if not found

% get_feedback/3:        returns feedback based on index match (correct, misplaced, incorrect)

start_game :-
    get_random_word(Word),
    game_ui(UserWord),
    game_loop(Word, UserWord, []).

game_loop(Word, Word, OverallFeedback) :- 
    finished_game(Word, OverallFeedback).

game_loop(Word, UserWord, OverallFeedback) :- 
    handle_user_input(Word, UserWord, 0, NewFeedback),
    append(OverallFeedback, [[UserWord, NewFeedback]], NewOverallFeedback),
    update_game_ui(NewOverallFeedback, NextWord),
    game_loop(Word, NextWord, NewOverallFeedback).

handle_user_input(_, [], _, []).

handle_user_input(Word, [U_Letter | U_Word], U_Index, [W_Feedback | NewAcc]) :-
    handle_letter(Word, U_Letter, 0, W_Index),
    get_feedback(U_Index, W_Index, W_Feedback),
    Next_Index is U_Index + 1,
    handle_user_input(Word, U_Word, Next_Index, NewAcc).

handle_letter([], _, _, -1).
handle_letter([Letter | _], Letter, W_Index, W_Index).
handle_letter([_ | R_Word], Letter, W_Index, Res) :-
    New_Index is W_Index + 1,
    handle_letter(R_Word, Letter, New_Index, Res).

get_feedback(_, -1, incorrect).
get_feedback(Res, Res, correct).
get_feedback(_, _, misplaced).

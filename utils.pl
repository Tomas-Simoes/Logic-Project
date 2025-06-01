:- module(utils, [
    handle_word/4,
    get_random_word/1, remove_last/2, color/2]).

:- use_module(words).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Shared utilities
% handle_word/4:         builds feedback list for the input word 
%                        expects target word, user word, current index, resulting feedback

% handle_letter/4:       finds index of letter if it exists in the word
%                        returns -1 if not found

% get_feedback/3:        returns feedback based on index match (correct, misplaced, incorrect)
handle_word(_, [], _, []).
handle_word(Word, [U_Letter | U_Word], U_Index, [W_Feedback | NewAcc]) :-
    handle_letter(Word, U_Letter, 0, W_Index),
    get_feedback(U_Index, W_Index, W_Feedback),
    Next_Index is U_Index + 1,
    handle_word(Word, U_Word, Next_Index, NewAcc).

handle_letter([], _, _, -1).
handle_letter([Letter | _], Letter, W_Index, W_Index).
handle_letter([_ | R_Word], Letter, W_Index, Res) :-
    New_Index is W_Index + 1,
    handle_letter(R_Word, Letter, New_Index, Res).

get_feedback(_, -1, incorrect).
get_feedback(Res, Res, correct).
get_feedback(_, _, misplaced).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities
get_random_word(Formatted) :-
    findall(X, word(X), WordList),
    random_member(ChoosenWord, WordList),
    string_chars(ChoosenWord, Formatted).

remove_last([], []).
remove_last([_], []).
remove_last([H|T], [H|NewArray]) :-
    remove_last(T, NewArray).
    
color(correct, green).
color(incorrect, red).
color(misplaced, yellow).
    

:- module(utils, [get_random_word/1, remove_last/2, color/2]).

:- use_module(words).

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
    

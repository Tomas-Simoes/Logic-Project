:- module(wordle_solver, [start_solver/0]).

:- use_module(utils).
:- use_module(words).
:- use_module(ui).

start_solver :-
    get_random_word(Word),
    
    findall(X, word(X), WordList),
    get_all_entropies(WordList, WordList, EntropyList),
    top_words(EntropyList, TopWords),
    solver_ui(TopWords, UserWord).

    
get_all_entropies([], _, []).
get_all_entropies([Word|RestWords], WordList, [[Word, WordEntropy]|RestEntropies]) :-
    exclude(=(Word), WordList, MatchList),
    get_patterns(Word, MatchList, Patterns),
    
    length(WordList, TotalWords),
    list_to_set(Patterns, UniquePatterns),
    get_pattern_probabilities(Patterns, UniquePatterns, TotalWords, Probabilities),
    
    calculate_entropy(Probabilities, WordEntropy),
    
    get_all_entropies(RestWords, WordList, RestEntropies).

get_patterns(_, [], []).
get_patterns(Word, [MatchWord|RestList], [MatchPattern|Patterns]) :-
    string_chars(MatchWord, MatchList),
    string_chars(Word, WordList),
    handle_word(WordList, MatchList, 0, MatchPattern),
    get_patterns(Word, RestList, Patterns).

get_pattern_probabilities(_, [], _, []).
get_pattern_probabilities(Patterns, [UniquePattern|RestUniques], TotalWords, [PatternProb|RestProb]) :-
    count_pattern(UniquePattern, Patterns, 0, PatternCount),
    PatternProb = PatternCount / TotalWords,
    get_pattern_probabilities(Patterns, RestUniques, TotalWords, RestProb).

count_pattern(Pattern, [], Acc, Acc).
count_pattern(Pattern, [Pattern|Rest], Acc, Count) :-
    NewCount is Acc + 1,
    count_pattern(Pattern, Rest, NewCount, Count).
count_pattern(Pattern, [_|Rest], Acc, Count) :-
    count_pattern(Pattern, Rest, Acc, Count).

calculate_entropy([], 0).
calculate_entropy([Probability|RestProb], Entropy) :-
    Probability > 0,
    calculate_entropy(RestProb, RestEntropy),
    Entropy is -Probability * log(Probability) / log(2) + RestEntropy.

top_words(EntropyList, TopWords) :-
    predsort(compare_entropy, EntropyList, Sorted),
    take(5, Sorted, TopWords).

compare_entropy(Order, [_, E1], [_, E2]) :-
    ( E1 > E2 -> Order = '<'        
    ; E1 < E2 -> Order = '>'
    ; Order = '<'                  
    ).
    
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Rest).
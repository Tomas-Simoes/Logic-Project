:- module(ui, [
    main_menu/0, 
    game_ui/1, update_game_ui/2, finished_game/2,
    solver_ui/2]).

:-use_module(library(ansi_term)).
:- use_module(wordle_game).
:- use_module(wordle_solver).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Main Menu
% main_menu: writes main menu and uses handle_choice/1 to handle the user input

main_menu :- 
    tty:menu("Welcome to UBIdle", 
        [
            start: "Start Game",
            solver: "Start Wordle Solver",
            options: "Options",
            exit: "Exit"
        ], Choice),
    handle_choice(Choice).

handle_choice(start) :-
    tty_clear,
    start_game.

handle_choice(solver) :-
    tty_clear,
    start_solver.

handle_choice(options) :-
    writeln("Showing options...").

handle_choice(exit) :-
    halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Game Interface
% game_ui/1:          outputs the game window when the game is initialized and waits for user input
%                     returns the user input "NextWord" as an array of chars

% update_game_ui/2:   outputs the game window after some user try with the previous user attemps 
%                     expects feedback of format [UserGuess, [incorrect_letter, correct_letter, ...]]
%                     returns the user input "NextWord" as an array of chars

% update_backspace/3: outputs the game window after the user backspaces it's input 
%                     expects feedback (same format as above) and the new user input as an array of chars
%                     unifies NextWord with the next input

% finished_game/2:    outputs the menu after the user has guessed the word
%                     expects the final word and the feedback (same format as above) 

game_ui(NextWord) :-
    tty_clear,
    writeln("Welcome to today's word!"),
    writeln("_  _  _  _  _"),
    write("Choose your word: "),

    read_from_ui([], [], Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, NextWord).

update_game_ui(Feedback, NextWord) :-
    tty_clear,
    writeln("Welcome to today's word!"),
    write_feedbacks(Feedback),
    writeln("_  _  _  _  _"),
    write("Choose your word: "),

    read_from_ui(Feedback, [], Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, NextWord).

update_backspace(Feedback, NewChars, NextWord) :-
    tty_clear,
    writeln("Welcome to today's word!"),

    write_feedbacks(Feedback),
    writeln("_  _  _  _  _"),
    write("Choose your word: "),
    string_chars(CurString, NewChars),
    write(CurString),

    read_from_ui(Feedback, NewChars, NextWord).

finished_game(Word, Feedback) :-
    length(Feedback, FB_Length),
    NumTries is FB_Length + 1,
    string_chars(String_Word, Word),
    
    format(atom(Title), "Congratulations, you found the word '~w' in ~w tries.~nWelcome to UBIdle", [String_Word, NumTries]),
    tty:menu(Title, 
        [
            menu: "Back to menu",
            exit: "Exit"
        ], Choice),
    handle_finished_choice(Choice).

handle_finished_choice(menu) :-
    tty_clear,
    main_menu.

handle_finished_choice(exit) :-
    halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Solver Interface

solver_ui(EntropyList, NextWord) :-
    tty_clear,

    create_entropy_strings(EntropyList, EntropyStrings),
    LeftColumn = ["Welcome to today's word!","_  _  _  _  _"],
    RightColumn = ["Solver best words"|EntropyStrings],

    print_to_columns(LeftColumn, RightColumn),
    
    write("Choose your word: "),
    read_from_ui(Feedback, [], Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, NextWord).

create_entropy_strings([], []).
create_entropy_strings([[Word, Entropy]|RestEntropy], [String|RestStrings]) :-
    format(string(String), "~w~t~8|~2f%%", [Word, Entropy]),
    create_entropy_strings(RestEntropy, RestStrings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
% write_feedbacks/1:    outputs the _ _ _ _ _ with correct letters and colors  
%                       expects a feedback of type [Word, [letters_feedback]]

% read_from_ui/3:       reads character by character and handles specific cases
%                       expects feedback (same format as above), an array of chars (with the user input)
%                       returns Final_Input as a string

% print_to_columns/2:   prints two lists of strings side by side as columns
%                       aligns the right column starting at column 30

write_feedbacks([]).
write_feedbacks([[[],[]]|RestFeedbacks]) :-
    writeln(""),
    write_feedbacks(RestFeedbacks).
write_feedbacks([[[Letter|Word], [L_Feedback|W_Feedback]]|RestFeedbacks]) :-
    color(L_Feedback, L_Color),
    ansi_format([fg(L_Color), underline], '~w', [Letter]),
    write("  "),
    write_feedbacks([[Word, W_Feedback]|RestFeedbacks]).

% needs Feedbacks to feed to update_after_backspace
read_from_ui(Feedbacks, Chars, Final_Input) :-
    get_single_char(Code),
    length(Chars, InputLength),
    ( 
    (Code =:= 10; Code =:= 13) ->                       % pressed enter
        ((InputLength < 5) ->
            read_from_ui(Feedbacks, Chars, Final_Input)
        ;
            string_chars(Final_Input, Chars))
    ;
    (Code =:= 8 ; Code =:= 127) ->                      % pressed backspace 
      tty_clear,
      remove_last(Chars, New_Chars),
      update_backspace(Feedbacks, New_Chars, Final_Input)
    ;
    (Code =:= 32) ->                                    % pressed space
        read_from_ui(Feedbacks, Chars, Final_Input)
    ;
    (InputLength + 1 > 5) ->                            % already has 5 letters
        read_from_ui(Feedbacks, Chars, Final_Input)
    ;                                                    % valid input
        char_code(Char, Code),
        write(Char),
        append(Chars, [Char], New_Chars),
        read_from_ui(Feedbacks, New_Chars, Final_Input)
    ).
    
print_to_columns([], []).
print_to_columns([], [HR|TR]) :-
    format("~|~t~30+ ~w~n", [HR]),
    print_to_columns([], TR).
print_to_columns([HL|TL], []) :-
    format("~w~n", [HL]),
    print_to_columns(TL, []).
print_to_columns([HL|TL], [HR|TR]) :-
    format("~|~w~t~30+ ~w~n", [HL, HR]),
    print_to_columns(TL, TR).

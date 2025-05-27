:- module(ui, [main_menu/0, game_ui/1, update_game_ui/2]).

:-use_module(library(ansi_term)).
:- use_module(wordle_game).
:- use_module(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Main Menu

main_menu :- 
    tty:menu("Welcome to UBIdle", 
        [
            start: "Start Game",
            solver: "Start Wordle Solver",
            options: "Options"
        ], Choice),
    handle_choice(Choice).

handle_choice(start) :-
    tty_clear,
    start_game.

handle_choice(solver) :-
    writeln("Starting Wordle Solver...").

handle_choice(options) :-
    writeln("Showing options...").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Game Interface
    
game_ui(Formatted) :-
    writeln("Welcome to today's word!"),
    writeln("_  _  _  _  _"),
    write("Choose your word: "),
    read_from_ui([], [], Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, Formatted).

update_game_ui(Feedback, NextWord) :-
    tty_clear,
    writeln("Welcome to today's word!"),

    write_feedbacks(Feedback),
    writeln(""),

    write("Choose your word: "),
    read_from_ui(Feedback, [], Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, NextWord).

update_after_backspace(Feedback, NewChars, NextWord) :-
    tty_clear,
    writeln("Welcome to today's word!"),

    write_feedbacks(Feedback),
    writeln(""),

    write("Choose your word: "),
    write(NewChars),
    read_from_ui(Feedback, NewChars, Input_Word),
    string_lower(Input_Word, Lower_Word),
    string_chars(Lower_Word, NextWord).

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
            read_from_ui([], Chars, Final_Input)
        ;
            string_chars(Final_Input, Chars))
    ;
    (Code =:= 8 ; Code =:= 127) ->                      % pressed backspace 
      tty_clear,
      remove_last(Chars, New_Chars),
      update_after_backspace(Feedback, New_Chars, Final_Input)
    ;
    (Code =:= 32) ->                                    % pressed space
        read_from_ui([], Chars, Final_Input)
    ;
    (InputLength + 1 > 5) ->                            % already has 5 letters
        read_from_ui([], Chars, Final_Input)
    ;                                              % valid input
        char_code(Char, Code),
        write(Char),
        append(Chars, [Char], New_Chars),
        read_from_ui([], New_Chars, Final_Input)
    ).
    
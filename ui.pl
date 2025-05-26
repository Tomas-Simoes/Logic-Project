:- module(ui, [main_menu/0]).

main_menu :- 
    tty:menu("Welcome to your favorite Wordle", 
        [
            start: "Start Game",
            solver: "Start Wordle Solver",
            options: "Options"
        ], Choice),
    handle_choice(Choice).

handle_choice(start) :-
    writeln("Starting game...").

handle_choice(solver) :-
    writeln("Starting Wordle Solver...").

handle_choice(options) :-
    writeln("Showing options...").

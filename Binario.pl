:-dynamic solve_cell/3.

size(6).

fixed_cell(0,2,x).
fixed_cell(1,2,x).
fixed_cell(2,0,x).
fixed_cell(2,5,x).
fixed_cell(3,2,o).
fixed_cell(4,1,x).
fixed_cell(4,5,x).
fixed_cell(5,0,o).
fixed_cell(5,4,o).




% CONCATENATORS
concatenate_row(Row, Result) :- % A predicate to concatenate a row into a string

    size(N),
    M is N - 1,
    findall(Value, (between(0, M, Column), (fixed_cell(Row, Column, Value); solve_cell(Row, Column, Value))), List),
    term_string(Result, List).

concatenate_column(Column, Result) :- % A predicate to concatenate a column into a string
    size(N),
    M is N - 1,
    findall(Value, (between(0, M, Row), (fixed_cell(Row, Column, Value); solve_cell(Row, Column, Value))), List),
    term_string(Result, List).
% END OF CONCATENATORS




% COUNT SYMBOL CORRECT
count_symbol(_, "", 0) :- !. % A predicate to count occurrences of a specific symbol
count_symbol(Symbol, String, Count) :- % Recursive case: count symbol occurrences in the string
    sub_string(String, 0, 1, _, Symbol), % Check if the first character is the symbol
    !,
    sub_string(String, 1, _, 0, Rest), % Get the rest of the string
    count_symbol(Symbol, Rest, SubCount),
    Count is SubCount + 1.
count_symbol(Symbol, String, Count) :-
    sub_string(String, 1, _, 0, Rest), % Skip the first character
    count_symbol(Symbol, Rest, Count).

strings_match(String1, String2) :- % A predicate to check if two strings match
    String1 = String2. % Pattern match to check if both strings are equal

equal_xo(String) :- % A predicate to check if the number of 'x' is equal to the number of 'o' in a string
    count_symbol('x', String, XCount),
    count_symbol('o', String, OCount),
    XCount = OCount.

symbol_count_correct:- % A predicate to check if the number of 'x' is equal to the number of 'o' in each row and column
    size(N),
    M is N-1,
    forall(between(0, M, Row), (concatenate_row(Row, RowString), equal_xo(RowString))), % Check rows
    forall(between(0, M, Column), (concatenate_column(Column, ColumnString), equal_xo(ColumnString))). % Check columns
% End of SYMBOL_COUNT_CORRECT




% ALL FILLED
all_filled :- % A predicate to check if all rows and columns are filled
    size(N),
    M is N - 1,
    \+ (
        between(0, M, Row),
        between(0, M, Column),
        (fixed_cell(Row, Column, n); solve_cell(Row, Column, n))
    ).
% END OF ALL FILLED




% NO TRIPLES
no_triples_in_row(Row):-concatenate_row(Row, Result),\+(check_triples(Result)). % A predicate to ensure that no row or column has three consecutive similar symbols.
no_triples_in_column(Column):-concatenate_column(Column, Result),\+(check_triples(Result)).

check_triples(String) :-
    string_chars(String, List), % Convert the string to a list of characters
    check_triples_helper(List). % Call the original predicate with the list

check_triples_helper([]) :- !. % Cut to prevent backtracking
check_triples_helper([X,X,X]) :- X \= n, !. % Cut to prevent backtracking and fail if X is n
check_triples_helper([X,X,X|_]) :- X \= n, !. % Cut to prevent backtracking and fail if X is n and succeed if there are more than three equal elements
check_triples_helper([_,B,C|T]) :- % Skip the first element
    check_triples_helper([B,C|T]). % Call with a smaller list

no_triples :- % A predicate to call no_third_equal_row and no_third_equal_column on each row and column (0 to 5)
size(N),
M is N-1,
forall(between(0, M, Row), no_triples_in_row(Row)),
forall(between(0, M, Column), no_triples_in_column(Column)).
% End OF NO TRIPLES




% NO REPEAT
no_rows_match :- % Predicate to check if any rows match and returns true/false
    size(N),
    M is N - 1,
    \+ (
        between(0, M, Row1),
        Row1_new is Row1 + 1,
        between(Row1_new, M, Row2),
        concatenate_row(Row1, String1),
        concatenate_row(Row2, String2),
        strings_match(String1, String2)
    ).

no_columns_match :- % Predicate to check if any columns match and returns true/false
    size(N),
    M is N - 1,
    \+ (
        between(0, M, Column1),
        Column1_new is Column1 + 1,
        between(Column1_new, M, Column2),
        concatenate_column(Column1, String1),
        concatenate_column(Column2, String2),
        strings_match(String1, String2)
    ).

no_repeat :- % A predicate that calls the no_columns_match and no_rows_match predicates, returning false if there is a match.
    no_rows_match,
    no_columns_match.
% END OF NO REPEAT


% INIT
loop :- 
    between(0, 6, Row), % Loop through all the indices and call solve_cell if the cell is not fixed 
    between(0, 6, Column), 
    \+ fixed_cell(Row, Column,_), 
    assert(solve_cell(Row, Column, n)), 
    fail. 

loop :- true. % Base case for the loop 

init:- retractall(solve_cell(_,_,_)),
    loop,
    print_board. 
% END OF INIT


% SET
set(Row,Column,Value):-
    retractall(solve_cell(Row,Column,_)),
    assert(solve_cell(Row,Column,Value)),
    print_board.
% END OF SET


% PRINT
print_board :-
    size(N),
    M is N - 1,
    print_horizontal_line(N),
    forall(between(0, M, Row), (
        print_row(Row),
        print_horizontal_line(N)
    )).

print_horizontal_line(N) :-
    format('+~|~t~`-t~*+=~|~n', [N]).

print_row(Row) :-
    size(N),
    M is N - 1,
    format('|', []),
    forall(between(0, M, Column), (
        (fixed_cell(Row, Column, Value); solve_cell(Row, Column, Value)),
        print_cell(Value)
    )),
    format('|~n', []).

print_cell(Value) :-
    var(Value),
    format('   ', []).
print_cell(Value) :-
    format(' ~w ', [Value]).
% END OF PRINT


% SOLVED
solved:- all_filled, no_triples, symbol_count_correct, no_repeat. %  A predicate to call the four rules that check that the solution is correct.
% END OF SOLVED



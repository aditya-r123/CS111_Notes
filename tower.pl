% 1. ntower
%N, a nonnegative integer specifying the size of the square grid.
%T, a list of N lists, each representing a row of the square grid. Each row is represented by a list of N distinct integers from 1 through N. The corresponding columns also contain all the integers from 1 through N.
%C, a structure with function symbol counts and arity 4. Its arguments are all lists of N integers, and represent the tower counts for the top, bottom, left, and right edges, respectively.

ntower(N, T, C) :-
    row_len(T, N),
    col_len(T, N),

row_len(T, N) :- 
    length(T, N).
    
col_len([], _).
col_len([H | T], N) :-
    length(H, N),
    col_len(T, N).

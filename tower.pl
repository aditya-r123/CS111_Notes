ntower(N, Grid, Counts) :-
    row_len(Grid, N),
    col_len(Grid, N),
    valid_range(Grid, N),
    maplist(fd_all_different, Grid),
    transpose(Grid, GridT),
    maplist(fd_all_different, GridT),
    Counts = counts(Top, Bottom, Left, Right),
    check_edges(GridT, Top),
    check_edges_reversed(GridT, Bottom),
    check_edges(Grid, Left),
    check_edges_reversed(Grid, Right).

row_len(Grid, N) :- 
    length(Grid, N).

col_len([], _).
col_len([Row | Rest], N) :-
    length(Row, N),
    col_len(Rest, N).

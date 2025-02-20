% 1. ntower

ntower(N, T, C) :-
    check_rowlength(T, N),
    check_columnlength(T, N),
    check_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    C = counts(Top, Bot, Left, Right),
    check_edge(TT, Top),
    check_edge_reverse(TT, Bot),
    check_edge(T, Left),
    check_edge_reverse(T, Right).

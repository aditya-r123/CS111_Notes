% 1. tower

tower(N, T, C) :-
    check_rowlen(T, N),
    check_collen(T, N),
    check_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    C = counts(Top, Bot, Left, Right),
    check_edge(TT, Top),
    check_edge_rev(TT, Bot),
    check_edge(T, Left),
    check_edge_rev(T, Right).

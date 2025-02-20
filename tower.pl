% 1. tower
tower(N, T, C) :-
    check_rowlen(T, N),
    check_collen(T, N),
    check_domain(T, N),

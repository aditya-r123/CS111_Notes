tower(N, T, C) :-
    check_rowlen(T, N),
    check_collen(T, N),
    check_domain(T),
    transpose(T, Transposed),
    check_edge(T, C.top),
    check_edge_rev(T, C.bottom),
    check_edge(Transposed, C.left),
    check_edge_rev(Transposed, C.right).

check_rowlen([], _).
check_rowlen([H|T], N) :-
    length(H, N),
    check_rowlen(T, N).

check_collen([], _).
check_collen([H|T], N) :-
    length(H, N),
    check_collen(T, N).

check_domain([]).
check_domain([H|T]) :-
    maplist(between(1, N), H),
    check_domain(T).

check_edge([], []).
check_edge([HD | TL], [VHD | VTL]) :-
    check_list(HD, VHD),
    check_edge(TL, VTL).

check_edge_rev([], []).
check_edge_rev([HD | TL], [VHD | VTL]) :-
    reverse(HD, RHD),
    check_list(RHD, VHD),
    check_edge_rev(TL, VTL).

check_list(L, V) :-
    count_visible(L, 0, V).

count_visible([], 0, _).
count_visible([HD | TL], Count, Max) :-
    (HD > Max -> NewCount is Count + 1, NewMax is HD ; NewCount is Count, NewMax is Max),
    count_visible(TL, NewCount, NewMax).

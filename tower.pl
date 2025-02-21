% Main predicate: ntower/3
ntower(N, T, C) :-
    N >= 0,  % Ensure N is non-negative
    check_rowlen(T, N),         % Check all rows have length N
    check_collen(T, N),         % Check all columns have length N
    check_domain(T, N),         % Ensure all values are between 1 and N
    transpose(T, Transposed),   % Transpose grid to handle columns
    check_edge(T, C.top),       % Check top edge counts
    check_edge_rev(T, C.bottom),% Check bottom edge counts
    check_edge(Transposed, C.left), % Check left edge counts
    check_edge_rev(Transposed, C.right). % Check right edge counts

% Ensure the length of each row is equal to N
check_rowlen([], _).
check_rowlen([H|T], N) :-
    length(H, N),
    check_rowlen(T, N).

% Ensure the length of each column is equal to N
check_collen([], _).
check_collen([H|T], N) :-
    length(H, N),
    check_collen(T, N).

% Ensure all values are between 1 and N
check_domain([],_).
check_domain([H|T], N) :-
    maplist(between(1, N), H),
    check_domain(T, N).

% Check the visible tower count for a row
check_edge([], []).
check_edge([HD | TL], [VHD | VTL]) :-
    check_list(HD, VHD),
    check_edge(TL, VTL).

% Check the reverse of the visible tower count for a row
check_edge_rev([], []).
check_edge_rev([HD | TL], [VHD | VTL]) :-
    reverse(HD, RHD),
    check_list(RHD, VHD),
    check_edge_rev(TL, VTL).

% Count the visible towers in a row
check_list(L, V) :-
    count_visible(L, 0, V).

% Count the number of visible towers from left to right in a row
count_visible([], 0, _).
count_visible([HD | TL], Count, Max) :-
    (HD > Max -> NewCount is Count + 1, NewMax is HD ; NewCount is Count, NewMax is Max),
    count_visible(TL, NewCount, NewMax).

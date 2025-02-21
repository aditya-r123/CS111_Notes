% Main rule
ntower(N, T, C) :-
    row_len(T, N),
    col_len(T, N),
    valid_range(T, N),
    maplist(fd_all_different, T),
    transpose(T, TTransposed),
    maplist(fd_all_different, TTransposed),
    C = counts(Top, Bottom, Left, Right),
    check_edges(TTransposed, Top),
    check_edges_reversed(TTransposed, Bottom),
    check_edges(T, Left),
    check_edges_reversed(T, Right).

row_len(T, N) :- 
    length(T, N).

col_len([], _).
col_len([Row | Rest], N) :-
    length(Row, N),
    col_len(Rest, N).

valid_range([], _).
valid_range([Row | Rest], N) :-
    fd_domain(Row, 1, N),
    valid_range(Rest, N).

transpose([], _, []).
transpose([_|Rows], Columns, [TransposedRow | TransposedRest]) :-
    extract_heads_tails(Columns, TransposedRow, RemainingColumns),
    transpose(Rows, RemainingColumns, TransposedRest).

extract_heads_tails([], [], []).
extract_heads_tails([[Head | Tail] | Rest], [Head | Heads], [Tail | Tails]) :-
    extract_heads_tails(Rest, Heads, Tails).

check_edges([], []).
check_edges([Row | Rest], [Visible | VisRest]) :-
    count_visible(Row, Visible),
    check_edges(Rest, VisRest).

check_edges_reversed([], []).
check_edges_reversed([Row | Rest], [Visible | VisRest]) :-
    reverse(Row, ReversedRow),
    count_visible(ReversedRow, Visible),
    check_edges_reversed(Rest, VisRest).

count_visible([], 0, _).
count_visible([Height | Rest], Count, Max) :-
    Height #> Max,
    count_visible(Rest, CountPrev, Height),
    Count is CountPrev + 1.
count_visible([Height | Rest], Count, Max) :-
    Height #=< Max,
    count_visible(Rest, Count, Max).

% 2. plain_tower

plain_tower(N, T, C) :-
    check_rowlen(T, N),
    C = counts(Top, Bot, Left, Right),
    process_rows(N, T, Left, Right),
    transpose(T, TT),
    process_rows(N, TT, Top, Bot).

process_rows(_, [], [], []).
process_rows(N, [HD | TL], [FHD | FTL], [BHD | BTL]) :-
    length(HD, N),
    maplist(between(1, N), HD),
    check_unique(HD),
    count_visible_plain(HD, 0, 0, FHD),
    reverse(HD, RHD),
    count_visible_plain(RHD, 0, 0, BHD),
    process_rows(N, TL, FTL, BTL).
    
check_unique(L):-
    sort(L, Sorted),
    length(L, Len1),
    length(Sorted, Len2),
    Len1 == Len2.
    
count_visible_plain([], Acc, _, Count) :- Count is Acc.
count_visible_plain([HD | TL], Acc, Max, Count) :-
    % HD is visible
    HD > Max,                                 
    NewAcc is Acc+1,
    count_visible_plain(TL, NewAcc, HD, Count).
count_visible_plain([HD | TL], Acc, Max, Count) :-
    % HD is not visible
    HD < Max,                                                                  
    count_visible_plain(TL, Acc, Max, Count).

% Performance Tests

test_tower(T) :-
    statistics(cpu_time, [Start | _]),
    tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

test_plain_tower(T) :-
    statistics(cpu_time, [Start | _]),
    plain_tower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

speedup(Ratio) :-
    test_tower(T1),
    test_plain_tower(T2),
    Ratio is T2/T1.	       
	       
% 3. ambiguous
    
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
    ntower(N, Grid1, C),
    ntower(N, Grid2, C),
    Grid1 \= Grid2.

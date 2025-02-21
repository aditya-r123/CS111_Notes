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

plain_ntower(N, Grid, Counts) :-
    row_len(Grid, N),
    Counts = counts(Top, Bottom, Left, Right),
    validate_rows(N, Grid, Left, Right),
    transpose(Grid, GridT),
    validate_rows(N, GridT, Top, Bottom).

validate_rows(_, [], [], []).
validate_rows(N, [Row | Rest], [LeftVisible | LeftRest], [RightVisible | RightRest]) :-
    length(Row, N),
    maplist(between(1, N), Row),
    all_unique(Row),
    count_visible_plain(Row, LeftVisible),
    reverse(Row, ReversedRow),
    count_visible_plain(ReversedRow, RightVisible),
    validate_rows(N, Rest, LeftRest, RightRest).

all_unique(List) :-
    sort(List, Sorted),
    length(List, OriginalLen),
    length(Sorted, UniqueLen),
    OriginalLen == UniqueLen.

count_visible_plain([], Count, _, FinalCount) :- 
    FinalCount is Count.
count_visible_plain([Height | Rest], Count, MaxHeight, FinalCount) :-
    Height > MaxHeight,                                 
    NewCount is Count + 1,
    count_visible_plain(Rest, NewCount, Height, FinalCount).
count_visible_plain([Height | Rest], Count, MaxHeight, FinalCount) :-
    Height =< MaxHeight,                                                                  
    count_visible_plain(Rest, Count, MaxHeight, FinalCount).

test_tower(Time) :-
    statistics(cpu_time, [Start | _]),
    ntower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    Time is (End - Start).

test_plain_tower(Time) :-
    statistics(cpu_time, [Start | _]),
    plain_ntower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    Time is (End - Start).

speedup(Ratio) :-
    test_tower(TimeOptimized),
    test_plain_tower(TimePlain),
    Ratio is TimePlain / TimeOptimized.

ambiguous(N, Counts, Grid1, Grid2) :-
    ntower(N, Grid1, Counts),
    ntower(N, Grid2, Counts),
    Grid1 \= Grid2.

% Main rule

ntower(N, T, C) :-
    row_len(T, N),
    col_len(T, N),
    range(T, N),
    maplist(fd_all_different, T),
    transpose(T, T2),
    maplist(fd_all_different, T2),
    C = counts(Top, Bottom, Left, Right),
    edge(T2, Top),
    edge_reverse(T2, Bottom),
    edge(T, Left),
    edge_reverse(T, Right).


row_len(T, N) :- 
    length(T, N).


col_len([], _).
col_len([H | T], N) :-
    length(H, N),
    col_len(T, N).
    


range([], _).
range([H | T], N) :-
    fd_domain(H, 1, N),
    range(T, N).

transpose([], []).
transpose([F|Fs], T) :-
    transpose(Fs, Ts),
    lists_firsts_rests([F|Fs], [H|T], Rest),
    transpose(Rest, T).
    
lists_firsts_rests([], [], []).
lists_firsts_rests([[H|_]|T], [H|Firsts], Rest) :-
    lists_firsts_rests(T, Firsts, Rest).
    
edge([], []).
edge([H | T], [H2 | T2]) :-
    check_list(H, H2),
    edge(T, T2).

edge_reverse([], []).
edge_reverse([H | T], [H2 | T2]) :-
    reverse(H, RH),
    visible_count(RH, H2),
    edge_reverse(T, T2).
    
verify(L, V) :-
    visible_count(L, Count, 0),
    V #= Count.

visible_count([], 0, _).
visible_count([HD | TL], Count, Max) :-
    HD #> Max,
    visible_count(TL, Count2, HD),
    Count is Count2+1.
visible_count([HD | TL], Count, Max) :-
    HD #< Max,
    visible_count(TL, Count, Max).

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

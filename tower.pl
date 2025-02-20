% 1. ntower
%N, a nonnegative integer specifying the size of the square grid.
%T, a list of N lists, each representing a row of the square grid. Each row is represented by a list of N distinct integers from 1 through N. The corresponding columns also contain all the integers from 1 through N.
%C, a structure with function symbol counts and arity 4. Its arguments are all lists of N integers, and represent the tower counts for the top, bottom, left, and right edges, respectively.

%main rule
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
    
transpose([], _, []).
transpose([_|Rs], Cs, [Tr|Ts]) :-
        lists_firsts_rests(Cs, Tr, Rs1),
        transpose(Rs, Rs1, Ts).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|R]|Rest], [F|Fs], [R|Rs]) :-
        lists_firsts_rests(Rest, Fs, Rs).
    
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
visible_count([H | T], Count, Max) :-
    H #> Max,
    visible_count(T, Count2, H),
    Count is Count2+1.
visible_count([H | T], Count, Max) :-
    H #< Max,
    visible_count(T, Count, Max).

test_ntower(T) :-
    statistics(cpu_time, [Start | _]),
    ntower(5, _,
         counts([2,3,2,1,4],
                [3,1,3,3,2],
                [4,1,2,5,2],
                [2,4,2,1,2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

         
ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C),
    ntower(N, T2, C),
    T1 \= T2.



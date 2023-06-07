zs_maximum_at([Max|T], Max, 0) :-
 max_list(T, Max0), Max >= Max0.
zs_maximum_at([H|T], Max, Pos) :-
    zs_maximum_at(T, Max0, Pos0),
    H < Max0,
    Max is Max0,
    Pos is Pos0 + 1.
zs_maximum_at([H|T], H, 0) :-
    max_list(T, Max), H >= Max.
zs_maximum_at([H|T], Max, Pos) :-
    zs_maximum_at(T, Max, Pos0),
    H < Max,
    Max is Max,
    Pos is Pos0 + 1.


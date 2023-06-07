find_max([], _, _, _) :- fail.

% base case: one element list
find_max([X], X, 1, 1).

% recursive case: find maximum element in list
find_max([X|Xs], Max, Pos, CurrPos) :-
    find_max(Xs, MaxRest, PosRest, CurrPosRest),
    (X > MaxRest -> Max = X, Pos = CurrPos ; Max = MaxRest, Pos = PosRest),
    CurrPos is CurrPosRest + 1.

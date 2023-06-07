% base case: empty list
find_max([], _, _, _) :- fail.

% base case: one element list
find_max([X], X, 1, 1).

% recursive case: find maximum element in list
find_max([X|Xs], Max, Pos, CurrPos) :-
    find_max(Xs, MaxRest, PosRest, CurrPosRest),
    (X > MaxRest -> Max = X, Pos = CurrPos ; Max = MaxRest, Pos = PosRest),
    CurrPos is CurrPosRest + 1.

% top-level predicate that wraps find_max/4 and handles starting position
find_max_wrapper(Xs, Max, Pos) :- find_max(Xs, Max, Pos, 1).

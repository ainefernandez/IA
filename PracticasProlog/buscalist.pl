consecutivos(A,B,[A,B|_]):-
	!. 
consecutivos(A,B,[_|R]):-
	d(A,B,R).

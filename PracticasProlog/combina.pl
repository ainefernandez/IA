
combina([], Lista, Lista):-
	!.
combina([X|Lista1,Lista2,[X|Lista3]):-
	combina(Lista1,Lista2,Lista3).
/*(i,i,o) 
 * (i,i,i)
 * (o,i,i)
 * (i,o,i)*/


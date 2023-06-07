/*valor_max(i,i,o):*/
/*
valor_max(X,Y,Z):-
	X>Y,Z is X.
valor_max(X,Y,Z):-
	X=<Y,Z is Y.
*/
/*Otra versión más eficiente*/
 valor_max(X,Y,X):-
	 X>Y.
 valor_max(X,Y,Y):-
	 X=<Y.


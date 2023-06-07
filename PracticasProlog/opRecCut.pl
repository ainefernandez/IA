/*Patrones de flujo posibles: 
 * valor_max(i,i,o): 
 * valor_max(i,o,i): 
 * valor_max(o,i,i): */
valor_max(X,Y,X):-
	X>Y,!.
valor_max(_,Y,Y).

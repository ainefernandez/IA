/* Este es un comentario en prolog */ 
hombre(jose).
mujer(maria).
hombre(juan). 
papa(juan,jose). 
papa(juan,maria). 
valioso(dinero). 
dificilDeObtener(dinero). 
le_da(pedro,libro,antonio).  
 /* Primer argumento quien, 2 argumento que, 3 argumento a quien  */ 
 
/* Regla, variables locales, misma variable en toda una misma regla, no se puede cambiar el valor de una variable*/ 
hermana (X,Y):-
 	papa(Z,X),
 	mujer(X),
 	papa(Z,Y),                             
 	X\==Y.
 	
hijo(X,Y):-
 	papa(Y,X),
 	hombre(X). 

humano(X):-
	mujer(X); 
	hombre(X). 
	
/* and=, or =; */ 

humano(X):-
	mujer(X). 
humano(X):-
	hombre(X). 


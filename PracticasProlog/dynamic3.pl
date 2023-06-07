:- dynamic grande/1,chico/1.
grande(bisonte).
grande(elefante).
mediano(oso).
chico(gato). 
chico(conejo). 

prog:-
	grande(elefante), 
	write("uno"), nl,
	retract(grande(elefante)), 
	not(grande(elefante)), 
	write("dos"), nl, 
	chico(gato),	
	chico(conejo), 
	write("tres"), nl,
	retractall(chico(_)), 
	write("cuatro"),nl, 
	not(chico(dato)), 
	not(chico(conejo)), 
	write("cinco"), nl.


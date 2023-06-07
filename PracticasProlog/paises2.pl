saludos:-
	prolog_pais(Pais1),
	write(Pais1),
	write(" saluda a:"),
	nl,
	!, 
	prolog_pais(Pais2), 
	Pais2\==Pais1, 
	write(Pais2),
	nl,
	fail.
prolog_pais(japon).
prolog_pais(francia). 
prolog_pais(hungria).
prolog_pais(bhutan).
prolog_pais(kenya).
prolog_pais(suriname).
main:-
	saludos.
main.

/*Japón saluda a todos los otros países sin saludarse a sí mismo por el cut en la parte del país1, el fail del final hace que se regrese hasta terminar todos los países*/ 
/* Ahora poner una segunda clausula no resuelve el problema del false porque se repite la segunda parte del programa*/
/*main hace que saludos te regrese true al final porque saludos es true porque si lo pudo hacer*/
/* Si le quitas el cut, Todos se saludan*/ 

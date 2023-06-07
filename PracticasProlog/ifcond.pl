clasifica(X):-
	write(X),nl, 
	(X<0->
		write("El argumento es negativo."),nl 
	;(X=:=0->
		write("El argumento es igual a cero."), nl
	; 	write("El argumento es positivo."),nl
	)
).
	


% Base de conocimiento con los casos
caso(1, 38, solucion(100, 20.0, madera)).
caso(2, 15, solucion(50, 501.38, concreto)).
caso(3, 78, solucion(100, 100.75, acero)).

% Predicado para calcular la diferencia de pisos
diferencia_pisos(NumPisos1, NumPisos2, Diferencia) :-
    Diferencia is abs(NumPisos1 - NumPisos2).

% Predicado para recuperar el caso más cercano
recupera(NumPisos, Solucion) :-
    caso(_, NumPisosCaso, SolucionCaso),
    diferencia_pisos(NumPisos, NumPisosCaso, Diferencia),
    Diferencia =< 5,
    Solucion is SolucionCaso,
    !.

% Si no se encuentra ningún caso cercano, mostrar mensaje de error
recupera(_, "No se encontró un caso cercano.").


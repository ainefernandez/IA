%ALEJANDRA ARREDONDO 189744, AINÉ FERNANDEZ 188680, DANYA GÓMEZ %198618, YULIANA PADILLA 188037. 

% Clase: Polinomio

:- module(polinomio, [polinomio/1, polinomio_toString/2]).

:- use_module(library(pce)).

:- pce_begin_class(polinomio, object,
                  "polinomio class").

variable(coef, list).
variable(degree, integer).

coef(P, Coef:list) :<-
    get(P, coef, Coef).

degree(P, Degree:integer) :<-
    get(P, degree, Degree).

polinomio(Coef) :-
    new(P, polinomio(Coef)).

polinomio(P, Coef:list) :->
    send(P, slot, coef, Coef),
    length(Coef, Degree),
    send(P, slot, degree, Degree - 1).

:- pce_end_class.

% toString

polinomio_toString(P, S) :-
    coef(P, Coef),
    degree(P, Degree),
    polinomio_toStringR(Coef, Degree, S).
polinomio_toStringR([], _, "0").
polinomio_toStringR([C|Cs], D, S) :-
    term_toString(C, Cs, D, T),
    (Cs == [] -> S = T ; polinomio_toStringR(Cs, D, Ts), format(atom(S), "~w + ~w", [T, Ts])).
term_toString(C, [], D, T) :-
(C == 0 -> T = "" ;
(C == 1 -> Cs = "" ; (C == -1 -> Cs = "-" ; format(atom(Cs), "~w", [abs(C)])),
(D == 0 -> format(atom(T), "ww", [Cs, ""]) ;
(D == 1 -> format(atom(T), "~wx", [Cs, ""]) ;
format(atom(T), "~wx^~w", [Cs, "", D]))))).
term_toString(C, _, D, T) :-
(C == 0 -> T = "" ;
(C == 1 -> Cs = "" ; (C == -1 -> Cs = "-" ; format(atom(Cs), "~w", [abs(C)])),
(D == 0 -> format(atom(T), "ww", [Cs, ""]) ;
(D == 1 -> format(atom(T), "~wx", [Cs, ""]) ;
format(atom(T), "~wx^~w", [Cs, "", D]))))).

% Consola de prueba, dentro del buffer
main:-
    use_module(polinomio),
    write("uno"),nl,
    polinomio([1, 0, -1], P),
    polinomio_toString(P, S).

% Suma de polinomios

plus(polinomio(CoefA, DegreeA), polinomio(CoefB, DegreeB), polinomio(CoefC, DegreeC)) :-
length(CoefC, MaxDegree),
findall(Coef, (between(0, max(DegreeA, DegreeB), I),
(nth0(I, CoefA, CoefAValue), nth0(I, CoefB, CoefBValue), CoefCValue is CoefAValue + CoefBValue), nth0(I, CoefC, CoefCValue)), CoefC),
polinomio_degree(polinomio(CoefC, _), DegreeC).

% Producto de polinomios

times(polinomio(CoefA, DegreeA), polinomio(CoefB, DegreeB), polinomio(CoefC, DegreeC)) :-
length(CoefC, MaxDegree),
MaxDegree is DegreeA + DegreeB,
findall(Coef, (between(0, DegreeA, I), between(0, DegreeB, J), Index is I + J, (nth0(I, CoefA, CoefAValue), nth0(J, CoefB, CoefBValue), CoefCValue is CoefAValue * CoefBValue), nth0(Index, CoefC, CoefCValue) ), CoefC),
polinomio_degree(polinomio(CoefC, _), DegreeC).



% Composición de polinomios, recursiva

compose(polinomio(CoefA, DegreeA), polinomio(CoefB, DegreeB), polinomio(CoefC, DegreeC)) :-
composeR(DegreeA, CoefA, CoefB, CoefC).

composeR(DegreeA, CoefA, CoefB, CoefC) :-
( DegreeA < 0 ->
CoefC = []
;
nth0(DegreeA, CoefA, Coef),
Term = [Coef, 0],
times(polinomio(CoefB, DegreeB), polinomio(CoefC, DegreeC), polinomio(TimesBC, _)),
plus(polinomio(Term, 0), polinomio(TimesBC, _), polinomio(NewC, _)),
NewDeg is DegreeA - 1,
composeR(NewDeg, CoefA, CoefB, NewC)).
% Resta de polinomios

minus(polinomio(CoefA, DegreeA), polinomio(CoefB, DegreeB), polinomio(CoefC, DegreeC)) :-
length(CoefC, MaxDegree),
findall(Coef, (between(0, max(DegreeA, DegreeB), I),
(nth0(I, CoefA, CoefAValue), nth0(I, CoefB, CoefBValue), CoefCValue is CoefAValue -  CoefBValue), nth0(I, CoefC, CoefCValue)), CoefC),
polinomio_degree(polinomio(CoefC, _), DegreeC).

% Derivada de polinomios
%Primera variable es la función 
%Segunda variable a que variable se va a derivar
%Tercera variable resultado

differentiate(C, X, 0):- number(C).	
%si la función es una constante regresa 0 

differentiate (X, X, 1). 		
%si la función está derivada a la misma variable regresa 1

differentiate(A+B, X, R1+R2):- differentiate(A, X, R1), differentiate(B, X, R2).
 %regla general para una derivada con suma. 

differentiate(A-B, X, R1-R2):- differentiate(A, X, R1), differentiate(B, X, R2). 
%regla general para una derivada con resta. 

differentiate(C*A, X, C*R1):- differentiate(A, X, R1), number(C). 
%regla general para una derivada de multiplicación con constante. 

differentiate(A*B, X, ((B*R1)+(A*R2)):- differentiate(A, X, R1), differentiate(B, X, R2). 
%regla de la multiplicación en derivadas

differentiate(A, X, (((R1*B)-(R2*A))/B^2)):- differentiate(A, X, R1), differentiate(B, X, R2). 
%regla general de la división en derivadas 

differentiate(X^N, X, (N*(X^N-1))):- number(N). 
%regla general para derivar funciones con exponentes. 

differentiate(A^X, X, ((A^X)*ln(A))).
%regla general para constantes elevadas a una función. 

differentiate(sin(A),X,cos(A)*R1):- differentiate(A,X,R1).
%regla general para derivadas de seno

differentiate(cos(A),X,-sin(A)*R1):- differentiate(A,X,R1).
%regla general para derivadas de coseno
	


% Evaluación de polinomios
evaluate(polinomio(CoefA,0), X,P):-
nth0(0,CoefA, C),  P is C.
evaluate(polinomio(CoefA,DegreeA), X,P):-
		evaluate(polinomio(CoefA,DegreeA1), X,P1),
		nth0(DegreeA1,CoefA, C), 
		P is C+X*P1,
		DegreeA is DegreeA1-1.





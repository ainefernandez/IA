:- use_module(library(lists)).
:- use_module(library(clpfd)).
:-dynamic mesa/1.
:-dynamic mis_fichas/1.

%INICIO DEL JUEGO

%Función bienvenida: 
%Esta función no tiene parámetros porque es con la que se inicia el juego 
%Le pregunta al usuario por sus fichas y las guarda en una lista, [MisFichasH|MisFichasT]
%Cabe resaltar que una ficha es una lista con el número extremo izquierdo como la cabeza y el número del extremo derecho la cola de la lista 
%Así que la lista de fichas es una lista de listas
%Luego llama a la función turno1 para continuar el juego 


bienvenida:-
    write("Bienvenido al juego. Suerte."),
    nl,
    write("Dame la lista de tus fichas, deben ser 7."),
    nl,
    read([MisFichasH|MisFichasT]), 
    nl,
    nl,
    turno1([MisFichasH|MisFichasT], 0,0,1). 
    %Lista de fichas del usuario, 0 puntos del usuario, 0 puntos del rival, partida número 1 
    
%Función turno1
%Párametros: 
  %[MisFichasH|MisFichasT]: Lista de fichas del usuario 
  %MisPuntos: Puntos del usuario 
  %RivPuntos: Puntos del rival 
  %Partida: Número de partida 
%Esta función le pregunta al usuario quien inició el juego
%Si el rival inició el juego debe insertar "rival" y si fue el usuario deberá insertar "usuario"
%Guarda quien inicio el juego en la variable Turno1 
%Luego llama a la función turno2
  
  turno1([MisFichasH|MisFichasT], MisPuntos, RivPuntos, Partida):-
    write("¿Quien inicio el juego? Rival (rival) o Usuario (usuario)"),
    nl,
    read(Turno1),
    nl,
    nl,
    turno2(Turno1, [MisFichasH|MisFichasT], MisPuntos, RivPuntos, Partida).
    
%Función turno2: 
%Parámetros: 
  %Turno1: Quien inicio el juego (rival o usuario)
  %[MFH|MFT]: Lista de fichas del usuario 
  %MisPuntos: Puntos del usuario 
  %RivPuntos: Puntos del rival 
  %Partida: Número de partida 
%Si el rival inició el juego entonces le pregunta al usuario que ficha jugó el rival 
%La guarda en una lista de fichas jugadas que se volverá 
%Si el rival inició el juego entonces le pregunta al usuario que ficha jugó el rival  
%La guarda en una lista de fichas jugadas que se volverá la mesa del dominó 
%Luego llama a la función jugar
%Esta función se llama recursivamente hasta el fin de la partida 

turno2(Turno1, [MFH|MFT], MisPuntos, RivPuntos, Partida):-
    Turno_1 == rival,
    write("¿Que ficha jugo el rival? "),
    nl,
    read([FJH|FJT]),
    nl,
    nl,
    jugar([MFH|MFT], 1, [[FJH|FJT]], 6, 14, 1, 0, MisPuntos, RivPuntos, Partida).
    %Lista de fichas del usuario, 1 significa que le toca jugar al rival, fichas jugadas (mesa)
    %6 son el número de fichas del rival porque ya jugó 1, 14 son las fichas en la sopa 
    %1 significa que todavía no es el final de la partida, 0 número de turnos pasados porque el rival si jugó

%Si el usuario inició el juego, le pregunta con qué ficha fue con la que inicio y la guarda en las fichas jugadas 
%Luego borra la ficha jugada de la lista de fichas que tiene usuario, la lista resultante se llama Nuevas
%Para terminar llama a la función jugar 
    
 turno2(Turno1, [MFH|MFT], MisPuntos, RivPuntos, Partida):-
    Turno_1 == usuario,
    write("¿Con que ficha iniciaste? "),
    nl,
    read([FJH|FJT]),
    nl,
    delete([MFH|MFT],[FJH|FJT],Nuevas),
    nl,
    jugar(Nuevas, 2, [[FJH|FJT]], 7, 14, 1, 0, MisPuntos, RivPuntos, Partida).
    %Lista de fichas del usuario, 2 significa que le toca jugar al usuario, fichas jugadas (mesa)
    %7 son el número de fichas del rival, 14 son las fichas en la sopa 
    %1 significa que todavía no es el final de la partida, 0 número de turnos pasados porque el usuario sí jugó
 
 %JUGAR DOMINÓ 
 
 %Función jugar
 %Parámetros: 
    %[MisFichasH|MisFichasT]: Lista de las fichas del usuario 
    %Turno: A quien le toca jugar 
    %[MesaH|MesaT]: Lista de fichas jugadas o mesa
    %Rival: Número de fichas que tiene el rival 
    %Sopa: Número de fichas en la sopa 
    %Fin: Si es el fin de la partida o no 
    %TPasados: Número de turnos pasados 
    %MisPuntos: Puntos del usuario 
    %RivPuntos: Puntos del rival 
    %Partida: Número de partida
 
 
 %Caso 1: 
 %El usuario sí puede jugar porque no es el fin del juego y le toca al usuario
 %Llama a la función tengoficha para evaluar si tiene alguna ficha para colocar en el tablero 
 jugar([MisFichasH|MisFichasT], Turno, [MesaH|MesaT], Rival, Sopa, Fin, _, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 1,
    Pozo > 0,
    tengoficha([MisFichasH|MisFichasT], [MesaH|MesaT]), 
    write("Te toca jugar"),                             
    nl,
    write("¿Que ficha quieres jugar?:"),
    calculo_minimax([MesaH|MesaT],[MisFichasH|MisFichasT], Rec),
    write("La IA recomienda jugar: "),
    write(Rec),
    nl,
    jugar_ficha([MisFichasH|MisFichasT], Rec, [MesaH|MesaT], MesaFT),
    nl,
    borrar_ficha(Rec, [MisFichasH|MisFichasT], MisFichasFT),
    nl,
    write("Tus fichas al final del turno: "),
    nl,
    write(MisFichasFT),
    nl,
    nl,
    write("Mesa al final del turno: "),
    nl,
    write(MesaFT),
    nl,
    nl,
    length(MisFichasFT,NMF), 
    fin_juego(0, Rival, NMF, Final), 
    jugar(MisFichasFT, 2, MesaFT, Rival, Pozo, Final, 0, MisPuntos, RivPuntos, Partida).


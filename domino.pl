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
  %



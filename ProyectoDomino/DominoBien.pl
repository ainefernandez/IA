%PROYECTO DE PROGRAMACIÓN 2:PROGRAMA PARA JUGAR DOMINÓ 
%ELABORADO POR: 
%DANYA GÓMEZ 
%YULIANA PADILLA 
%ALEJANDRA ARREDONDO
%AINÉ FERNÁNDEZ 
/*********************************************************************************************************************************/

 %EL PROGRAMA CONSTA DE LAS SIGUIENTES PARTES: 

 %INICIO DEL JUEGO: Funciones para iniciar el juego, así como el primer turno y los turnos subsecuentes
 %JUGAR DOMINÓ: Funcciones para jugar dominó, robar, jugar, pasar etc.  
 %FUNCIONES AUXILIARES PARA JUGAR DOMINÓ: Funciones que tratan con las situaciones del juego del dominó, como la inversión de fichas, 
 %tirar fichas,verificar si se acabó el juego, donde poner las fichas etc. 
 %INICIO DE NUEVA PARTIDA: Funciones que toman en cuenta el resultado de la partida anterior para iniciar una nueva partida
 % y contar los puntos al final. Este programa está diseñado para jugar 3 partidas antes de determinar el ganador del juego,
 % pero esto puede ser modificado con facilidad. 
 %FUNCIÓN HEURÍSTICA: Determina el valor heurístico de cada ficha para escoger la estrategia de juego del usuario

 /**********************************************************************************************************************************/



:- use_module(library(lists)).
:-dynamic mesa/1.
:-dynamic mis_fichas/1.




 /**********************************************************************************************************************************/

%INICIO DEL JUEGO

%Función bienvenida: 
%Esta función no tiene parámetros porque es con la que se inicia el juego 
%Le pregunta al usuario por sus fichas y las guarda en una lista, [MisFichasH|MisFichasT]
%Cabe resaltar que una ficha es una lista con el número extremo izquierdo como la cabeza y el número del extremo derecho la cola de la lista 
%Así que la lista de fichas es una lista de listas
%Luego llama a la función turno1 para continuar el juego 

bienvenida:-
    write("Bienvenido al juego. Suerte!"),
    nl,
    write("Dame la lista de tus fichas."),
    nl,
    read([MisFichasH|MisFichasT] ), 
    nl,
    nl,
    turno1([MisFichasH|MisFichasT] , 0,0,1).
    

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



turno1([MisFichasH|MisFichasT], MisPuntos,  RivPuntos, Partida):-
    write("¿Quién inicio el juego? Ellos(rival) o Tú (usuario)"),
    nl,
    read(Turno1),
    nl,
    nl,
    turno2(Turno1, [MisFichasH|MisFichasT], MisPuntos,  RivPuntos, Partida).


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


turno2(Turno1, [MFH|MFT], MisPuntos,  RivPuntos, Partida):-
    Turno1 == rival,
    write("¿Qué ficha jugó el rival? "),
    nl,
    read([FJH|FJT]),
    nl,
    nl,
    jugar([MFH|MFT], 1, [[FJH|FJT]], 6, 14, 1, 0, MisPuntos,  RivPuntos, Partida).

%Si el usuario inició el juego, le pregunta con qué ficha fue con la que inicio y la guarda en las fichas jugadas 
%Luego borra la ficha jugada de la lista de fichas que tiene usuario, la lista resultante se llama Nuevas
%Para terminar llama a la función jugar 

turno2(Turno1, [MFH|MFT], MisPuntos,  RivPuntos, Partida):-
    Turno1 == usuario,
    write("¿Qué ficha jugaste? "),
    nl,
    read([FJH|FJT]),
    nl,
    delete([MFH|MFT],[FJH|FJT],Nuevas),
    nl,
    jugar(Nuevas, 2, [[FJH|FJT]], 7, 14, 1, 0, MisPuntos,  RivPuntos, Partida).

 /**********************************************************************************************************************************/


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

%Caso 1: Usuario juega
 %El usuario sí puede jugar porque no es el fin del juego y le toca al usuario
 %Llama a la función tengo para evaluar si tiene alguna ficha para poner en el Mesa 
 %La IA recomienda una ficha para jugar y se llama a la función jugarF para poner la ficha en la mesa 
 %Luego se borra la ficha jugada de las del usuario 
 %Al final cuenta las fichas que le quedan al usuario, evalúa si es el fin del juego y llama a la función jugar para iniciar el turno del rival 

jugar([MisFichasH|MisFichasT], Turno, [MesaH|MesaT], Rival, Sopa, Fin, _, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 1,
    Sopa > 0,
    tengo([MisFichasH|MisFichasT], [MesaH|MesaT]), 
    write("Tu turno."),                            
    nl,
    minimax([MesaH|MesaT],[MisFichasH|MisFichasT], FJ),
    write(" La IA va a jugar la ficha: "),
    write(FJ),
    nl,
    jugarF([MisFichasH|MisFichasT], FJ, [MesaH|MesaT], MesaFT),
    nl,
    borrarF(FJ, [MisFichasH|MisFichasT], MisFichasFT),
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
    acabo(0, Rival, NMF, Final), 
    jugar(MisFichasFT, 2, MesaFT, Rival, Sopa, Final, 0, MisPuntos, RivPuntos, Partida).


%Caso 2: Usuario no puede jugar, pero puede robar 
    %El usuario no puede jugar pero puede robar de la sopa 
    %Llama a la función no_tengo  para comprobar que el usuario no tiene fichas para jugar, entonces el usuario roba de la sopa 
    %Le indica que ficha robó, la función la guarda en robo y la agrega a la lista de fichas del usuario
    %Luego vuelve a llamar a jugar con las fichas al final del turno del usuario  
    %Este caso se llama hasta que encuentra una ficha que si puede jugar 

jugar([MisFichasH|MisFichasT], Turno, [MesaH|MesaT], Rival, Sopa, Fin, Tpasados, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 1,
    Sopa > 0,
    no_tengo([MisFichasH|MisFichasT], [MesaH|MesaT]),
    write("Tu turno."),
    nl,
    write("No puedes jugar ninguna ficha."),
    nl,
    nl,
    write("¿Qué ficha robaste?"),
    nl,
    read(FichaRob),
    nl,
    nl,
    append([FichaRob],[MisFichasH|MisFichasT],NuevasMisF), 
    SopaFT is Sopa- 1,
    jugar(NuevasMisF, 1, [MesaH|MesaT], Rival, SopaFT, Fin, Tpasados, MisPuntos, RivPuntos, Partida).

%Caso 3: Usuario no puede jugar, tampoco puede robar 
%El usuario no tiene ficha para jugar y la sopa está vacía, pasa de turno 
%Llama a la función no_tengo para comprobar que el usuario no tiene fichas para jugar y como la sopa está vacía, pasa de turno
%Al final se toma en cuenta que pasó de turno y se comprueba si acabó la partida y llama otra vez a la función jugar para iniciar el turno del rival  


jugar([MisFichasH|MisFichasT], Turno, [MesaH|MesaT], Rival, Sopa, Fin, Tpasados, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 1,
    Sopa=:= 0,
    no_tengo([MisFichasH|MisFichasT], [MesaH|MesaT]),
    write("Tu turno."),
    nl,
    write("No puedes jugar ninguna ficha y no hay fichas que robar."),
    nl,
    write("Pasas de turno"),
    nl,
    nl,
    write("Tus fichas al final del turno: "),
    nl,
    write([MisFichasH|MisFichasT]),
    nl,
    nl,
    write("Mesa al final del turno: "),
    nl,
    write([MesaH|MesaT]),
    nl,
    nl,
    length([MisFichasH|MisFichasT],NMF),
    TpasadosFT is Tpasados + 1, 
    acabo(TpasadosFT, Rival, NMF, Final),
    jugar([MisFichasH|MisFichasT], 2, [MesaH|MesaT], Rival, Sopa, Final, TpasadosFT, MisPuntos, RivPuntos, Partida).


%Caso 4: Rival puede jugar, pasar y robar 
%Si el rival robó fichas, le preguntamos al usuario cuántas robó, si jugó actualiza la mesa, si pasó actualiza el número de turnos pasados, verifica si es el final de la partida y vuelve a llamar a jugar para iniciar el turno del usuario 

jugar(MisFichas, Turno, Mesa, Rival, Sopa, Fin, Tpasados, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 2,
    Sopa> 0,
    write("Turno del rival."),
    nl,
    write("¿El rival robó fichas? Si(si) o No(no)"),
    nl,
    read(Robo), % Lee si robó fichas o no
    nl,
    robo_rival(Robo, Robadas), % En caso de haber robado, esta función lee cuántas.
    nl,
    write("¿Jugó alguna ficha el rival? Si(si) o No(no)"),
    nl,
    read(FJR),
    nl,
    turno_rival(FJR, Mesa, MesaFT), % Si jugó una ficha, lee
    jugor(FJR,FichasJugadasR),                     % cual y regresa el Mesa nuevo.
    nl,
    write("El rival se queda con : "),
    FichasRivalFT is Rival + Robadas -FichasJugadasR , % Actualiza fichas del rival.
    write(FichasRivalFT),
    write(" fichas."),
    nl,
    nl,
    write("Mesa al final del turno: "),
    nl,
    write(MesaFT),
    nl,
    nl,
    length(MisFichas,NMF),
    tpasados_r(Tpasados,FJR,TpasadosFT), 
    acabo(TpasadosFT, FichasRivalFT, NMF, Final),
    SopaFT is Sopa- Robadas, 
    jugar(MisFichas, 1, MesaFT, FichasRivalFT, SopaFT, Final, TpasadosFT, MisPuntos, RivPuntos, Partida).

% Caso 5: El rival puede jugar o pasar, pero la sopa está vacía 
jugar(MisFichas, Turno, Mesa, Rival, Sopa, Fin, Tpasados, MisPuntos, RivPuntos, Partida):-
    Fin =:= 1,
    Turno =:= 2,
    Sopa=:= 0, 
    write("¿Jugó alguna ficha el rival? Si(si) o No(no)"), 
    nl,
    read(FJR),
    nl,
    turno_rival(FJR,Mesa, MesaFT),
   jugor(FJR,FichasJugadasR),
    nl,
    write("El rival se queda con: "),
    FichasRivalFT is Rival -FichasJugadasR ,
    write(FichasRivalFT),
    write(" fichas."),
    nl,
    nl,
    write("Mesa al final del turno: "),
    nl,
    write(MesaFT),
    nl,
    nl,
    length(MisFichas,NMF),
    tpasados_r(Tpasados, FJR,TpasadosFT),
    acabo(TpasadosFT, FichasRivalFT, NMF, Final),
    jugar(MisFichas, 1, MesaFT, FichasRivalFT, Sopa, Final, TpasadosFT, MisPuntos, RivPuntos, Partida).

%Caso 6: Fin de la partida
%Evalúa si la partida terminó en empate o quien es el ganador  
jugar(MisF, _, Mesa, Rival, Sopa, Fin, _, MisPuntos, RivPuntos, Partida):-
    Fin =:= 0,
    write("Fin de la Partida."),
    length(MisF,X),
    nl,
    nl,
    hay_ganador(X, Rival, PuntosUsuarioN, PuntosRivalN, Empate), 
    nl,nl,                                                          
    write("Resultados finales de la Partida: "),
    nl,nl,
    write("Mesa final: "),
    write(Mesa),
    nl,
    write("Tus fichas: "),
    write(MisF),
    nl,
    write("El rival se quedó con "),
    write(Rival),
    write(" fichas."),
    nl,
    write("La Sopa se quedó con "),
    write(Sopa),
    write(" fichas."),
    nl,
    nl,
    FinU is MisPuntos + PuntosUsuarioN,
    FinR is RivPuntos + PuntosRivalN,
    PartidaF is Partida + 1,
    ganador(PuntosUsuarioN, PuntosRivalN, Ganador), 
    iniciar_partida(FinU,FinR,Empate,PartidaF, Ganador).

 /**********************************************************************************************************************************/


%FUNCIONES AUXILIARES PARA JUGAR DOMINÓ

%Función turno_rival
%Parámetros: 
%FJR: jugó ficha el rival o no 
%Mesa: Mesa al inicio del turno 
%MesaNueva: Mesa al final del turno 
%Recibe la ficha que jugó el rival, comprueba que se puede jugar y la coloca en la mesa, al final se actualiza la mesa 
turno_rival(FJR, Mesa, MesaNueva):-
    FJR == si,
    write("¿Qué ficha jugó el rival?"),
    nl,
    read(FichaJR),
    nl,
    tengo([FichaJR], Mesa),
    cabeza(Mesa,[A|_]),
    cola(Mesa,[_|D]),
    donde_poner(FichaJR, [A|D], Pos),
    poner(FichaJR, Mesa, Pos, MesaNueva).
    % Se da la variable MesaNueva para que esa sea la que se regrese
    % en la llamada a la función juego pasa rival.

% Si no jugó una ficha, queda igual la mesa
turno_rival(FJR, Mesa, Mesa):-
    FJR == no.

%Función robo_rival
% Evalúa, si robó el rival, cuántas robó. Si no robó, marca 0.
% Recibe el input de si robó o no, regresa las robadas.
robo_rival(Robo, Robadas):-
    Robo == si,
    write("¿Cuantas fichas robo el rival?"),
    nl,
    read(InputRob),
    nl,
    Robadas is InputRob.

robo_rival(Robo, Robadas):-
    Robo == no,
    Robadas is 0.




%Función pasaR
%Parámetros: 
%FJR: jugó ficha el rival o no 
%Y: valor que nos ayuda a ver si el rival pasó o no 
% Si pasa el rival, marca 1. Si no pasa, marca 0
%Facilita llevar la cuenta de los turnos pasados 
pasar(FJR,Y):-
    FJR == si,
    Y is 0.
pasar(FJR,Y):-
    FJR == no,
    Y is 1.

%Función jugoR
%Parámetros: 
%FJR: jugó ficha el rival o no 
%Z: valor que nos ayuda a ver si el rival pasó o no 
% Si jugó una ficha el rival, marca 1. Si no, marca 0
%Facilita llevar la cuenta de las fichas del rival 
jugor(FJR,Z):-
    FJR == si,
    Z is 1.
jugor(FJR,Z):-
    FJR == no,
    Z is 0.

% Función no_tengo
%Parámetros: 
	%Misf: Las fichas del usuario 
	%Mesa: Mesa o fichas jugadas 
% Marca true si la ficha o lista de fichas recibida no puede ser jugada en la 
% mesa recibida.
no_tengo(Misf,Mesa):-
    cabeza(Mesa,[A|_]),
    cola(Mesa,[_|D]),
    no_tengo_r(Misf,[A|D]).
    % Llama a una función recursiva con las fichas y las orillas de la mesa.
 

% Caso base 
%Cuando se recibe una lista vacía no se puede jugar.

 no_tengo_r([MFH|MFT],_):-
    length([MFH|MFT], X),
    X=:=0.
    
% Caso recursivo 
%Va haciendo la lista de fichas cada vez más pequeña comprobando ficha por ficha si se puede jugar en alguno de los extremos.

no_tengo_r([MFH|MFT],Orillas):-
    length([MFH|MFT], X),
    X>1,
    no_igual(Orillas,MFH),
    no_tengo_r(MFT,Orillas).
    
% Cuando llega a la última ficha, comprueba si se puede jugar o no 
no_tengo_r(Fichas,Orillas):-
    length(Fichas, X),
    X=:=1,
    cabeza(Fichas,Y),
    no_igual(Orillas,Y).


%Funcion tengo
%Parámetros: 
%Misf: Las fichas del usuario 
	%Mesa: Mesa o fichas jugadas 
% Marca true si se puede jugar una ficha en la mesa
%La función recursiva recibe las orillas del Mesa y la lista de fichas.


tengo(Misf,Mesa):-
    cabeza(Mesa,[A|_]),
    cola(Mesa,[_|D]),
    tengo_r(Misf,[A|D]).
  
  % Si no es igual la ficha actual, sigue buscando   
 tengo_r([MFH|MFT],Orillas):-
    length([MFH|MFT], X),
    X>1,
    no_igual(Orillas,MFH),
    tengo_r(MFT,Orillas).
  
% Si la ficha actual es igual, termina de buscar y regresa true. 
 tengo_r([MFH|MFT],Orillas):-
    length([MFH|MFT], X),
    X>1,
    igual(Orillas,MFH).
  
 % Cuando queda una ficha, revisa si es igual o no.   
 tengo_r(Fichas,Extremos):-
    length(Fichas, X),
    X=:=1,
    cabeza(Fichas,Y),
    igual(Extremos,Y).
    
 tengo_r(Mis_fichas,_):-
    length(Mis_fichas, X),
    X=:=0,
    1>2. 
    
 %Función cabeza-cola: Consigue la cabeza y cola de una lista 
 cabeza([A|_],A).
 cola(List,B):-
    reverse(List,ListR),
    cabeza(ListR,B).
 
%Función igual
%Parámetros: 
%[A|B]: ficha 
%[C|D]: Mesa, C y D son las orillas de la mesa 
%Regresa true si algún valor de la ficha es igual a alguna orilla.
 igual([A|B],[C|D]):-
    A =:= C;
    A =:= D;
    B =:= C;
    B =:= D.
%Función no_igual
%Parámetros: 
%[A|B]: ficha 
%[C|D]: Mesa, C y D son las orillas de la mesa
% Regresa true si ningún valor de la ficha es igual a las orillas. 
 no_igual([A|B],[C|D]):-
    A =\= C,
    A =\= D,
    B =\= C,
    B =\= D.
 

%Función ganador
%Parámetros: 
%MisPuntos: puntos del usuario 
%RivPuntos: puntos del rival 
%Ganador: Quien es el ganador de la partida   
% Al terminar la partida y determina quién debe tirar primero en la próxima partida, %regresa 1 si el usuario gana, 2 si el rival gana, y 0 si es empate.
ganador(MisPuntos,  RivPuntos, Ganador):-
    MisPuntos>RivPuntos,
    Ganador is 1.

ganador(MisPuntos,  RivPuntos, Ganador):-
    MisPuntos<RivPuntos,
    Ganador is 2.
ganador(MisPuntos,  RivPuntos, Ganador):-
    MisPuntos=:=RivPuntos,
    Ganador is 0.

%Función hay_ganador
%Parámetros: 
	%NumMisF: Número de fichas que tiene el usuario 
	%NumRivalF: Número de fichas que tiene el rival 
	%MisPuntos: Puntos del usuario 
	%RivPuntos: Puntos del rival 
	%Partida: Número de partida 
% Regresa los puntos y si hay empate. Si es empate da 1, si no 0.


% Caso 1: Usuario gana
hay_ganador(NumMisF,_,MisPuntos,  RivPuntos, Empate):-
    NumMisF =:= 0,
    write("Ganamos la partida!!!"),
    MisPuntos is 3,
    RivPuntos is 0,
    Empate is 0.

%Caso 2: Rival gana
hay_ganador(_,NumRivalF, MisPuntos,  RivPuntos, Empate):-
    NumRivalF =:= 0,
    write("Perdimos la partida:("),
    MisPuntos is 0,
    RivPuntos is 3,
    Empate is 0.

%Caso 3: Empate 
hay_ganador(NumMisF,NumRivalF, MisPuntos,  RivPuntos, Empate):-
    NumMisF =\=0,
    NumRivalF =\= 0,
    write("Empatamos la Partida :0"),
    MisPuntos is 1,
    RivPuntos is 1,
    Empate is 1.

% Función turno
%Parámetros: 
%Turn: De quien es el turno
%Imprime de quién es el turno
turno(Turn):-
    Turn =:= 2,
    write("Es turno del rival.").

turno(Turn):-
    Turn =:= 1,
    write("Es tu turno.").


%Función jugarF
%Parámetros: 
%MisFichas: Fichas que tiene el usuario 
%Ficha: Ficha a ser jugada 
%Mesa: Mesa o fichas jugadas 
%MesaNueva: Mesa actualizada con la ficha ya jugada 
% Comprueba si se puede jugar la ficha seleccionada por el usuario, comprueba donde se puede poner, la pone y regresa la mesa actualizada
jugarF(MisFichas, Ficha, Mesa, MesaNueva):-
    tengo([Ficha], Mesa), %Pregunta específicamente si puedes jugar dicha ficha. 
    member(Ficha,MisFichas), % Comprueba que sea miembro de las fichas
    cabeza(Mesa,[A|_]),
    cola(Mesa,[_|D]),
    donde_poner(Ficha, [A|D], Orilla), % Regresa el lado del Mesa donde se puede poner.
    poner(Ficha, Mesa, Orilla, MesaNueva).

%Este caso toma en cuenta si la ficha está al revés 
jugarF(MisFichas, Ficha, Mesa, MesaNueva):-
    tengo([Ficha], Mesa), 
    reverse(Ficha, FichaR), % En caso de que esté al revés la ficha,
    member(FichaR,MisFichas), % comprueba que esté en la lista de fichas.
    cabeza(Mesa,[A|_]),
    cola(Mesa,[_|D]),
    donde_poner(FichaR, [A|D],Orilla),
    poner(FichaR, Mesa, Orilla, MesaNueva).
 
%Función poner 
%Parámetros: 
%[A|B]: Ficha
%Mesa: Mesa o fichas jugadas 
%Pos: Donde y como poner la ficha 
%MesaNueva: Mesa actualizada con la ficha jugada 
% Pone la ficha del Mesa donde corresponde e invierte la ficha en caso de que se %necesite 
% Si Pos es 1, se pone en la cabeza y no se invierte
%Si Pos es 2 se pone en la cabeza y se invierte 
 % Si Pos es 3 si es cola y no se invierte 
%Si Pos es 4 si es cola y se invierte
%Si Pos es 5 se puede poner en ambas y no se invierte, 
%Si Pos es6 se puede poner  en ambas y se invierte


 poner([A|B],Mesa, Pos, MesaNueva):-
    Pos =:=1,
    invertirF([A|B],Pos,Ficha),
    append([Ficha],Mesa,MesaNueva).

poner([A|B],Mesa, Pos, MesaNueva):-
    Pos =:=2,
    invertirF([A|B],Pos,Ficha),
    append([Ficha],Mesa,MesaNueva).

poner([A|B],Mesa, Pos, MesaNueva):-
    Pos=:=5,
    invertirF([A|B],Pos,Ficha),
    % append([Ficha],Mesa,MesaNueva).
    append(Mesa,[Ficha],MesaNueva).

poner([A|B],Mesa, Pos, MesaNueva):-
    Pos=:=6,
    invertirF([A|B],Pos,Ficha),
     append([Ficha],Mesa,MesaNueva).
% append(Mesa,[Ficha],MesaNueva).


poner([A|B],Mesa, Pos, MesaNueva):-
    Pos =:=3,
    invertirF([A|B],Pos,Ficha),
    append(Mesa,[Ficha],MesaNueva).

poner([A|B],Mesa, Pos, MesaNueva):-
    Pos =:=4,
    invertirF([A|B],Pos,Ficha),
    append(Mesa,[Ficha],MesaNueva).
 
%Función invertirF
%Parámetros: 
%Ficha: Ficha a invertir 
%Pos: Donde colocar la ficha 
%Nueva: Ficha invertida  
 invertirF(Ficha,Pos,Nueva):-
    par(Pos),
    reverse(Ficha,Nueva).
 invertirF(Ficha, _,Ficha).
 
% Función donde_poner 
%Parámetros: 
	%Ficha: Ficha a poner 
	%Orillas: Extremos de la mesa 
	%Pos: Donde poner la ficha 
%De acuerdo a la ficha y los extremos, regresa qué caso es para saber donde poner %la ficha 

 donde_poner(Ficha,Orillas,Pos):-
    a_d(Ficha,Orillas),
    b_c(Ficha, Orillas),
    Pos is 5.

 
 donde_poner(Ficha,Orillas,Pos):-
    b_d(Ficha, Orillas),
    a_c(Ficha,Orillas),
    Pos is 6.


 donde_poner(Ficha,Orillas, Pos):-
    b_c(Ficha,Orillas),
    Pos is 1.
 

 donde_poner(Ficha, Orillas, Pos):-
    a_c(Ficha,Orillas),
    Pos is 2.
 
 donde_poner(Ficha, Orillas, Pos):-
    a_d(Ficha,Orillas),
    Pos is 3.
 
 donde_poner(Ficha, Orillas, Pos):-
    b_d(Ficha,Orillas),
    Pos is 4.

% Revisa qué caso es, compara los extremos de la ficha con las orillas de la mesa 


 a_d([A|_], [_|D]):-
    A=:=D.
 
 b_c([_|B], [C|_]):-
    B=:=C.
 
 b_d([_|B], [_|D]):-
    B=:=D.
 
 a_c([A|_], [C|_]):-
    A=:=C.
    
 % Revisa si un número es par.
 par(N):-
    mod(N,2) =:= 0.

%Función borrarF
%Párametros: 
	%Ficha: Ficha jugada 
	%Viejas: Fichas con la ficha jugada
	%Nuevas: Fichas sin la ficha jugada   
%Actualiza las fichas con la ficha jugada,borra la ficha jugada,  invirtiendo la ficha en caso necesario.
borrarF(Ficha, Viejas, Nuevas):-
    member(Ficha,Viejas),
    delete(Viejas,Ficha,Nuevas).

borrarF(Ficha, Viejas, Nuevas):-
    reverse(Ficha, FichaR),
    member(FichaR,Viejas),
    delete(Viejas, FichaR, Nuevas).

%Función acabo
%Parámetros: 
%Tpasados: Número de turnos pasados anteriormente 
%NumRivalF: Número de fichas que tiene el rival 
%NumMisFichas: Número de fichas que tiene el usuario
%Fin: Si es el final del juego o no 
% Regresa 0 si ya se acabó el juego, 1 si sigue en curso.Verifica si el número
% de Tpasados seguidas es 2, si el rival ya no tiene fichas o si el jugador no tiene.
%se considera un empate 

%Pasaron los dos, se acabó el juego
acabo(Tpasados, _, _, Fin):-
    Tpasados =:=2,
    Fin is 0.

%Rival ya no tiene fichas 
acabo(_, NumRivalF, _, Fin):-
    NumRivalF =:= 0,
    Fin is 0.

%Usuario ya no tiene fichas 
acabo(_, _, NumMisFichas, Fin):-
    NumMisFichas =:=0,
    Fin is 0.

%No se acabó el juego 
acabo(Tpasados, NumRivalF, NumMisFichas, Fin):-
    NumMisFichas =\=0,
    NumRivalF =\= 0,
    Tpasados =\=2,
    Fin is 1.

%Función fin_juego
%Parámetros: 
	%MisPuntos: Puntos del usuario 
	%RivPuntos: Puntos del rival 
%Determina el ganador final del juego o si hay un empate en puntos después de 3 %partidas, el número de partidas en un juego se puede modificar fácilmente  

fin_juego(MisPuntos,  RivPuntos):-
    MisPuntos > RivPuntos,
    write("Eres el ganador del juego!!!").

fin_juego(MisPuntos,  RivPuntos):-
    MisPuntos < RivPuntos,
    write("Perdiste el juego :,(").

fin_juego(MisPuntos,  RivPuntos):-
    MisPuntos =:= RivPuntos,
    write("El resultado final es un empate.").

% Función tpasados
%Parámetros: 
	%TpasadosAnt: Turnos anteriormente pasados 
	%FJR: Si o no jugó el rival 
	%TP: Turnos pasados después del turno actual 
%Verifica el número de turnos pasados, si un jugador jugó se vuelve 0 

tpasados_r(TpasadosAnt, FJR, TP):-
    FJR == no,
    TpasadosAnt =:=1,
    TP is 2.

tpasados_r(_, FJR, TP):-
    FJR == si,
    TP is 0.
tpasados_r(TpasadosAnt,FJR, TP):-
    FJR == no,
    TpasadosAnt =:= 0,
    TP is 1.


 /**********************************************************************************************************************************/



%INICIO DE NUEVA PARTIDA

%Función iniciar_partida
% Función que se ejecuta a partir de terminar la segunda partida. Sirve para iniciar %una nueva partida mientras no haya terminado el juego, toma en cuenta las %condiciones en las que terminó la partida anterior para iniciar la nueva partida 

 % Caso 1: Partida anterior fue un empate
iniciar_partida(MisPuntos,  RivPuntos, Empate, Partida, _):-
    Empate =:= 1, 
    Partida < 4,   
    nl,
    nl,
    write("Inicio de la Partida numero: "),
    write(Partida),
    nl,
    nl,
    write("Dame la lista de tus fichas."),
    nl,
    read([MisFichasH|MisFichasT]),
    nl,
    turno1([MisFichasH|MisFichasT], MisPuntos,  RivPuntos, Partida). 
 

% Caso 2:  Usuario ganó la partida anterior 
iniciar_partida(MisPuntos,  RivPuntos, _, Partida, Ganador):-
    Ganador =:= 1, 
    Partida < 4,     % y no ha terminado el juego.
    nl,
    nl,
    write("Inicio de la Partida número: "),
    write(Partida),
    nl,
    nl,
    write("Dame la lista de tus fichas."),
    nl,
    read([MFH|MFT]),
    nl,
    write("¿Con qué ficha inicias?"),
    nl,
    read([FJH|FJT]),
    nl,
    nl,
    delete([MFH|MFT],[FJH|FJT],Nuevas),
    write(Nuevas),
        jugar(Nuevas, 2, [[FJH|FJT]], 7, 14, 1, 0, MisPuntos, RivPuntos, Partida).
 
%Caso 3:  Ganó el rival la partida anterior 
iniciar_partida(MisPuntos,  RivPuntos,_,  Partida, Ganador):-
    Ganador =:= 2, 
    Partida < 4,     
    nl,
    nl,
    write("Inicio de la Partida numero: "),
    write(Partida),
    nl,
    nl,
    write("Dame la lista de tus fichas."),
    nl,
    read([MFH|MFT]),
    nl,
    write("¿Qué ficha jugó el rival? "),
    nl,
    read([FJH|FJT]),
    nl,
    nl,
    jugar([MFH|MFT], 1, [[FJH|FJT]], 6, 14, 1, 0, MisPuntos,  RivPuntos, Partida).
    % Se llama a la función jugar.

%Fin del juego 
iniciar_partida(MisPuntos, RivPuntos, _, Partida, _):-
    Partida =:= 4, % Caso en el que ya se jugaron las 3 Partidas.
    nl,
    nl,
    write("Fin del juego."),
    nl,
    write("Puntos finales del usuario: "),
    write(MisPuntos),
    nl,
    write("Puntos finales del rival: "),
    write(RivPuntos),
    nl,
    fin_juego(MisPuntos, RivPuntos). 


 /**********************************************************************************************************************************/


%FUNCIÓN HEURÍSTICA

%Función suma_listas: 
%Parámetros: 
%3 listas: 2 para sumar y otra para poner el resultado 
% Esta función recibe dos listas de números y suma los valores entrada a entrada. 
% Regresa el resultado en otra lista.

suma_listas([], [], []).
suma_listas([H1|T1],[H2|T2],[X|L3]):-
   suma_listas(T1,T2,L3), 
   X is H1+H2.



%Función orillas 
%Parámetros: 
%[[H1,_]|T]: Mesa 
%Orilla_izq: Orilla izquierda de la mesa 
%Orilla_der: Orilla derecha de la mesa 
%Esta función recibe una lista de listas que es la mesa y regresa las orillas  de la %mesa para saber qué valores se pueden jugar siguiente.

orillas([[H1,_]|T], Orilla_izq, Orilla_der):-
   Orilla_izq = H1,
   last([_|T], X),
   last(X,Orilla_der).

 

%Función ficha_num
%Parámetros: 
	%MisFichas: Fichas del usuario
	%Numero_que_tengo: Número en la ficha 
	%Otros_numeros: Número en el otro extremo de la ficha 
% Recibe una lista de listas que son mis fichas.
% Sobre ella itera para saber si tenemos valores en la otra cara de la ficha regresa los valores.
% convierte_num convierte los resultados en una lista para que sea más fácil trabajar con ellos

ficha_num(MisFichas, Numero_que_tengo, Otros_numeros):-
   member([Numero_que_tengo, Otros_numeros], MisFichas).

ficha_num(MisFichas, Numero_que_tengo, Otros_numeros):-
   member([Otros_numeros, Numero_que_tengo], MisFichas).

convierte_num(MisFichas, Numero_que_tengo, Lista):-
   findall(Y, ficha_num(MisFichas, Numero_que_tengo, Y), Lista).


%Función mula
%Parámetros: 
%[[H1, H2]|T]: Lista con fichas 
%Numero: Numero que se quiere verificar 
%Revisa si hay mulas en una lista de listas.
% Recibe fichas y el número que se busca

mula([[H1, H2]|T], Numero):-
   (
      H1 == Numero, H2 == Numero -> 
         true
         ;
         mula(T, Numero)
   ).

encuentra_mulas([], []).

encuentra_mulas([[H1, H2]|T], [Mula1 | Mula2]):-
   encuentra_mulas(T, Mula2),
   (
      H1 == H2 ->
         Mula1 is H1;
         true
   ).

%Función jugadas
%Parámetros: 
%Mesa: Lista de fichas de la mesa 
%Mis fichas: Lista de fichas del usuario 
%FichasJug: Lista de fichas visibles para el usuario 
% Recibe las fichas de la mesa y las fichas del usuario y las junta en una lista que son las fichas visibles para el usuario 
% La funcion jugadas_con_num \3 recibe una lista de fichas, un número buscado, y regresa el total de fichas que tienen ese número
% La funcion jugadas_con_num \4 recibe dos listas de fichas que son después juntadas y llama a jugadas_con_num \3 con esa

jugadas(Mesa, MisFichas, FichasVis):-
   append(Mesa, MisFichas, FichasVis).

jugadas_con_num(FichasVis, Numero, Total):-
   convierte_num(FichasVis, Numero, Aux_lista),
   length(Aux_lista, Total).

jugadas_con_num(Numero, Total):-
   mesa(X),
   mis_fichas(Y), 
   jugadas(X, Y, FichasVis),
   jugadas_con_num(FichasVis, Numero, Aux_total),
   (
      hay_mula(FichasVis, Numero) ->
         Total is Aux_total - 1
         ;

         Total = Aux_total
   ).

jugadas_con_num(Lista1, Lista2, Numero, Total):- 
   jugadas(Lista1, Lista2, FichasVis),
   jugadas_con_num(FichasVis, Numero, Aux_total),
   (
      hay_mula(FichasVis, Numero) ->
         Total is Aux_total - 1
         ;

         Total = Aux_total
   ).

%Función suma_extremos
%Parámetros: 
%Lista1: Ficha 
%Lista 2: Resultado de la suma de los extremos de la ficha 
% Suma los valores de los extremos de las fichas 
% Regresa el resultado en una lista

suma_extremos([], []).
suma_extremos([[N1, N2] | T], [Sum1 | SumR]):-
   suma_extremos(T, SumR),
   Sum1 is N1 + N2.


%Función valor_heuristico
%Parámetros: 
%[Num1, Num2]: Ficha
%Numextremo: Número extremo
%Total_mias_con_num: Fichas del usuario con ese número 
%Jugadas_con_num: Fichas en la mesa con el número 
%Valor: Valor heurístico que se le va a asignar a la ficha
% Asigna un valor heurístico a cada ficha, se le da prioridad a las mulas y no nos importa los números en las fichas como tal, ya que nuestro único 
% objectivo es quedarnos sin fichas. 
% La funcion recorre_valor_heuristico regresa una lista con los valores heurísticos correspondientes a cada ficha

valor_heuristico([Num1, Num2], Numextremo, Total_mias_con_num, Jugadas_con_num, Valor):-
   Num1 == Num2,
   Num1 == Numextremo,
   Valor is 100 * Total_mias_con_num * Jugadas_con_num,
   !.

valor_heuristico([Num1 | _],Numextremo, Total_mias_con_num, Jugadas_con_num, Valor):-
   Num1 == Numextremo,
   Valor is 10 * Total_mias_con_num * Jugadas_con_num,
   !.

valor_heuristico([_ , Num2 | _],Numextremo, Total_mias_con_num, Jugadas_con_num, Valor):-
   Num2 == Numextremo,
   Valor is 10 * Total_mias_con_num * Jugadas_con_num,
   !.

valor_heuristico([_|_], _, _, _, Valor):-
   Valor is 0.


recorre_valor_heuristico([], _, _, _, []).

recorre_valor_heuristico([H|T], Valor_extremo, Total_mias_con_num, Jugadas_con_num, [Valor1|Valor2]):-
   recorre_valor_heuristico(T, Valor_extremo, Total_mias_con_num, Jugadas_con_num, Valor2),
   valor_heuristico(H, Valor_extremo, Total_mias_con_num, Jugadas_con_num, Valor1).



%Función_Heurística
% Mesa: Fichas en el Mesa
% MisFichas Fichas que tiene el usuario
% Valheuristicos:Lista con valores heurísitcos para cada ficha

% Esta función llama a todas las funciones anteriores para calcular el valor heurístico de cada una de las fichas y 
% regresar una lista con todos los valores heurísticos

funcion_heuristica([], MisFichas, Valheuristicos):-
   suma_fichas(MisFichas, Valheuristicos),
   !.

funcion_heuristica([[Valor_izquierdo,Valor_derecho]], MisFichas,  Valheuristicos):-
   jugadas([[Valor_izquierdo, Valor_derecho]], MisFichas, FichasVis),
   jugadas_con_num(FichasVis, Valor_izquierdo, Jugadas_con_num_izq),
   jugadas_con_num(FichasVis, Valor_derecho, Jugadas_con_num_der),
   convierte_num(MisFichas, Valor_izquierdo, Aux_izq),
   length(Aux_izq, Total_fichas_mias_izq),
   convierte_num(MisFichas, Valor_derecho, Aux_der),
   length(Aux_der, Total_fichas_mias_der),
   recorre_fichas_asignando_valor(MisFichas, Valor_izquierdo, Total_fichas_mias_izq, Jugadas_con_num_izq, L1),
   recorre_fichas_asignando_valor(MisFichas, Valor_derecho, Total_fichas_mias_der, Jugadas_con_num_der, L2),
   suma_listas(L1, L2,  Valheuristicos),
   !.

funcion_heuristica(Mesa, MisFichas,  Valheuristicos):-
   /* Obtenemos los números de los extremos */
   orillas(Mesa, Valor_izquierdo, Valor_derecho),
   /* Vemos qué fichas que se han jugado */
   jugadas(Mesa, MisFichas, Jugadas),
   /* Contamos el número de fichas que se han jugado con cada extremo */
   jugadas_con_num(Jugadas, Valor_izquierdo,  Jugadas_con_num_izq),
   jugadas_con_num(Jugadas, Valor_derecho, Jugadas_con_num_der),
   /* Calculamos cuántas fichas tiene el usuario con cada valor extremo */
   convierte_num(MisFichas, Valor_izquierdo, Aux_izq),
   convierte_num(MisFichas, Valor_derecho, Aux_der),
   length(Aux_izq, Total_fichas_mias_izq),
   length(Aux_der, Total_fichas_mias_der),
   /* Sacamos los valores heurísticos */
   recorre_valor_heuristico(MisFichas, Valor_izquierdo, Total_fichas_mias_izq, Jugadas_con_num_izq, L1),
   recorre_valor_heuristico(MisFichas, Valor_derecho, Total_fichas_mias_der,Jugadas_con_num_der, L2),
   suma_listas(L1, L2, Valheuristicos).



% Función minimax
% Parámetros:
% Mesa: Fichas en la mesa 
% MisFichas: Fichas del usuario 
% ValMax: Valor heurístico máximo  
% La función minimax aplica el algoritmo minimax, busca en la lista de valores heurísticos que produjo la función heurística 
% por el valor máximo de los valores heurísticos y así encontrar la ficha asociada a ese valor, y maximizar la jugada del usuario 
% Al final recomienda la ficha asociada para ser jugada. 


%Función max_position
% Función auxiliar que nos permite encontrar el valor máximo de una lista. 

max_position(List, Position) :-
    max_list(List, Max),
    nth0(Position, List, Max).

%Función match 
%Esta función nos ayuda a encontrar la ficha asociada al valor heurístico máximo y así poder recomendarla para jugar 

match([H|_],0,H) :-
        !.
match([_|T],N,H) :-
    N > 0,
    N1 is N-1,
    match(T,N1,H).


minimax(Mesa,MisFichas,ValMax):-
    funcion_heuristica(Mesa, MisFichas, R),
    max_position(R, Pos),
    match(MisFichas, Pos, ValMax).

 /**********************************************************************************************************************************/




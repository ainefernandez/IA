# -*- coding: utf-8 -*-
"""
Created on Mon May  8 23:59 2023

@author: Danya Gómez, Ainé Fernández, Alejandra Arredondo, Yuliana Padilla
"""

# Juego que usa una función heurística para jugar poker
# estilo Texas Hold'Em para dos personas ("heads up")

import random

class Player:
    def __init__(self):
        self.hand = []
        self.apuesta = 0

class Table:
    def __init__(self):
        self.lista_comun = []

mesa=Table()
current_player=Player()

# Recordemos que las cartas con value=14 corresponden a los aces
deck = list()
deck = [(valor, palo) for valor in range(2, 15) for palo in ["corazones", "diamantes", "treboles", "picas"]]

random.shuffle(deck)
current_player.hand = deck[:2]

for card in current_player.hand:
    deck.remove(card)

#%%
# Etapa Pre-Flop

print("Tus cartas son: ", current_player.hand)

def evaluar_pre_flop(hand):
    valores = [card[0] for card in hand]
    palos = [card[1] for card in hand]
    
    # Aquí sólo evaluamos el par dado
    suma = 0
    for valor in valores:
        suma+=valor
    apuesta = 0
    
    if ((suma>=2) and (suma<8)):
        apuesta = 100
    else:
        if ((suma>=8) and (suma<13)):
            apuesta = 200
        else:
            if ((suma>=13) and (suma<18)):
                apuesta = 300
            else:
                if ((suma>=18) and (suma<23)):
                    apuesta = 400
                else:
                    if ((suma>=23) and (suma<28)):
                        apuesta = 500
                    else:
                        return("Error")
    
    
    if(valores[0]==valores[1]) or (palos[0]==palos[1]):
        apuesta=apuesta*1.2
    return apuesta

current_player.apuesta = evaluar_pre_flop(current_player.hand)
print("Recomiendo apostar ", current_player.apuesta)

apuesta_oponente=input("¿Cuánto apostó tu oponente?: ")

#%%
# Etapa Flop

random.shuffle(deck)
mesa.lista_comun = deck[:3]

print("Las cartas comunes en la mesa son: ", mesa.lista_comun)

for card in mesa.lista_comun:
    deck.remove(card)

def evaluate_hand(hand):
    values = [card[0] for card in hand]
    suits = [card[1] for card in hand]
    pairs = 0
    three_of_a_kind = 0
    straight = False
    flush = False
    full_house = False
    four_of_a_kind = False
    straight_flush = False

    # Contar pares y tríos
    for value in set(values):
        if values.count(value) == 2:
            pairs += 1
        elif values.count(value) == 3:
            three_of_a_kind += 1
        elif values.count(value) == 4:
            four_of_a_kind = True

    # Verificar escalera
    if len(set(values)) == 5 and max(values) - min(values) == 4:
        straight = True

    # Verificar color
    if len(set(suits)) == 1:
        flush = True

    # Verificar full house
    if pairs == 1 and three_of_a_kind == 1:
        full_house = True

    # Verificar escalera de color
    if straight and flush:
        straight_flush = True
  
    # Asignar valor a la mano
    if straight_flush:
        return 100
    elif four_of_a_kind:
        return 75
    elif full_house:
        return 50
    elif flush:
        return 25
    elif straight:
        return 20
    elif three_of_a_kind:
        return 10
    elif pairs == 2:
        return 5
    elif pairs == 1:
        return 2
    else:        
        return 0
    
def evaluate_type(hand):
    values = [card[0] for card in hand]
    suits = [card[1] for card in hand]
    pairs = 0
    three_of_a_kind = 0
    straight = False
    flush = False
    full_house = False
    four_of_a_kind = False
    straight_flush = False

    # Contar pares y tríos
    for value in set(values):
        if values.count(value) == 2:
            pairs += 1
        elif values.count(value) == 3:
            three_of_a_kind += 1
        elif values.count(value) == 4:
            four_of_a_kind = True

    # Verificar escalera
    if len(set(values)) == 5 and max(values) - min(values) == 4:
        straight = True

    # Verificar color
    if len(set(suits)) == 1:
        flush = True

    # Verificar full house
    if pairs == 1 and three_of_a_kind == 1:
        full_house = True

    # Verificar escalera de color
    if straight and flush:
        straight_flush = True
  
    # Asignar valor a la mano
    if straight_flush:
        return ("una escalera de color")
    elif four_of_a_kind:
        return ("un poker (four of a kind)")
    elif full_house:
        return ("un full house")
    elif flush:
        return ("un color")
    elif straight:
        return ("una escalera")
    elif three_of_a_kind:
        return ("una tercia")
    elif pairs == 2:
        return ("un doble par")
    elif pairs == 1:
        return ("un par")
    else:        
        return ("ninguna en especial")

lista=list()

for card in current_player.hand:
    lista+=[card]
    
for card in mesa.lista_comun:
    lista+=[card]

def evaluar_flop(current_player, hand):
    valor = evaluate_hand(hand)
    if ((valor>=0) and (valor<2)):
        apuesta=current_player.apuesta
    else:
        if ((valor>=2) and (valor<10)):
            apuesta=current_player.apuesta*1.2
        else:
            if ((valor>=10) and (valor<25)):
                apuesta=current_player.apuesta*1.4
            else:
                if ((valor>=25) and (valor<50)):
                    apuesta=current_player.apuesta*1.6
                else:
                    if ((valor>=50) and (valor<75)):
                        apuesta=current_player.apuesta*1.8
                    else:
                        if valor>=75:
                            apuesta=current_player.apuesta*2
                        else:
                            return("Error")
    return apuesta

current_player.apuesta = evaluar_flop(current_player, lista)
print("Tu única mano posible es ", lista,", que es ", evaluate_type(lista))

print("Recomiendo apostar ", current_player.apuesta)

apuesta_oponente=input("¿Cuánto apostó tu oponente?: ")

#%%
# Etapa Turn

random.shuffle(deck)
mesa.lista_comun += deck[:1]

print("Las cartas comunes en la mesa son: ", mesa.lista_comun)

lista=list()

for card in current_player.hand:
    lista+=[card]
    
for card in mesa.lista_comun:
    lista+=[card]

posibles_manos=list()
if len(lista)==6:
    posibles_manos=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]]]
    
if len(lista)==7:
    posibles_manos=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[3], lista[6]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[6]],
                    [lista[0], lista[1], lista[2], lista[5], lista[6]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[6]],
                    [lista[0], lista[1], lista[3], lista[5], lista[6]],
                    [lista[0], lista[1], lista[4], lista[5], lista[6]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[6]],
                    [lista[0], lista[2], lista[3], lista[5], lista[6]],
                    [lista[0], lista[2], lista[4], lista[5], lista[6]],
                    [lista[0], lista[3], lista[4], lista[5], lista[6]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[6]],
                    [lista[1], lista[2], lista[3], lista[5], lista[6]],
                    [lista[1], lista[2], lista[4], lista[5], lista[6]],
                    [lista[1], lista[3], lista[4], lista[5], lista[6]],
                    [lista[2], lista[3], lista[4], lista[5], lista[6]]]

def evaluar_turn(current_player, posibles_manos):
    valor=0
    mejor_mano=posibles_manos[0]
    for hand in posibles_manos:
        if evaluate_hand(hand)>=valor:
            mejor_mano=hand
            valor=evaluate_hand(hand)
    apuesta=0
    if ((valor>=0) and (valor<2)):
        apuesta=current_player.apuesta
    else:
        if ((valor>=2) and (valor<10)):
            apuesta=current_player.apuesta*1.2
        else:
            if ((valor>=10) and (valor<25)):
                apuesta=current_player.apuesta*1.4
            else:
                if ((valor>=25) and (valor<50)):
                    apuesta=current_player.apuesta*1.6
                else:
                    if ((valor>=50) and (valor<75)):
                        apuesta=current_player.apuesta*1.8
                    else:
                        if valor>=75:
                            apuesta=current_player.apuesta*2
                        else:
                            return("Error")
    print("\nLa mejor mano que puedes jugar es ", mejor_mano, "porque es ", evaluate_type(mejor_mano))
    return(apuesta)

def evaluar_mejor_valor(current_player, posibles_manos):
    valor=0
    mejor_mano=posibles_manos[0]
    for hand in posibles_manos:
        if evaluate_hand(hand)>=valor:
            mejor_mano=hand
            valor=evaluate_hand(hand)
    return(valor)

current_player.apuesta = evaluar_turn(current_player, posibles_manos)

print("Recomiendo apostar ", current_player.apuesta)

apuesta_oponente=input("¿Cuánto apostó tu oponente?: ")

#%%
# Etapa River

random.shuffle(deck)
mesa.lista_comun += deck[:1]

print("Las cartas comunes en la mesa son: ", mesa.lista_comun)

lista=list()

for card in current_player.hand:
    lista+=[card]
    
for card in mesa.lista_comun:
    lista+=[card]

posibles_manos=list()
if len(lista)==6:
    posibles_manos=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]]]
    
if len(lista)==7:
    posibles_manos=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[3], lista[6]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[6]],
                    [lista[0], lista[1], lista[2], lista[5], lista[6]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[6]],
                    [lista[0], lista[1], lista[3], lista[5], lista[6]],
                    [lista[0], lista[1], lista[4], lista[5], lista[6]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[6]],
                    [lista[0], lista[2], lista[3], lista[5], lista[6]],
                    [lista[0], lista[2], lista[4], lista[5], lista[6]],
                    [lista[0], lista[3], lista[4], lista[5], lista[6]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[6]],
                    [lista[1], lista[2], lista[3], lista[5], lista[6]],
                    [lista[1], lista[2], lista[4], lista[5], lista[6]],
                    [lista[1], lista[3], lista[4], lista[5], lista[6]],
                    [lista[2], lista[3], lista[4], lista[5], lista[6]]]

# En esta etapa, la función heurística sería evaluar_river
# pero la función es la misma que evaluar_turn

current_player.apuesta = evaluar_turn(current_player, posibles_manos)

print("Recomiendo apostar ", current_player.apuesta)

apuesta_oponente=input("¿Cuánto apostó tu oponente?: ")


#%%
# Evaluación final, ¿duplico mi apuesta?

# Designamos una variable que predice el máximo valor posible de las cartas del oponente

deck = sorted(deck, key=lambda x: x[0], reverse=True)
possible_opponent = 0
possible_deck = deck[:2]
for card in possible_deck:
    possible_opponent+=card[0]
    
possible_opponent=possible_opponent/28

# Luego, calculamos su "mejor mano" posible

lista=list()

for card in possible_deck:
    lista+=[card]
    
for card in mesa.lista_comun:
    lista+=[card]

if len(lista)==6:
    posibles_manos_op=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]]]
    
if len(lista)==7:
    posibles_manos_op=[[lista[0], lista[1], lista[2], lista[3], lista[4]],
                    [lista[0], lista[1], lista[2], lista[3], lista[5]],
                    [lista[0], lista[1], lista[2], lista[3], lista[6]],
                    [lista[0], lista[1], lista[2], lista[4], lista[5]],
                    [lista[0], lista[1], lista[2], lista[4], lista[6]],
                    [lista[0], lista[1], lista[2], lista[5], lista[6]],
                    [lista[0], lista[1], lista[3], lista[4], lista[5]],
                    [lista[0], lista[1], lista[3], lista[4], lista[6]],
                    [lista[0], lista[1], lista[3], lista[5], lista[6]],
                    [lista[0], lista[1], lista[4], lista[5], lista[6]],
                    [lista[0], lista[2], lista[3], lista[4], lista[5]],
                    [lista[0], lista[2], lista[3], lista[4], lista[6]],
                    [lista[0], lista[2], lista[3], lista[5], lista[6]],
                    [lista[0], lista[2], lista[4], lista[5], lista[6]],
                    [lista[0], lista[3], lista[4], lista[5], lista[6]],
                    [lista[1], lista[2], lista[3], lista[4], lista[5]],
                    [lista[1], lista[2], lista[3], lista[4], lista[6]],
                    [lista[1], lista[2], lista[3], lista[5], lista[6]],
                    [lista[1], lista[2], lista[4], lista[5], lista[6]],
                    [lista[1], lista[3], lista[4], lista[5], lista[6]],
                    [lista[2], lista[3], lista[4], lista[5], lista[6]]]
    
    
def evaluar_oponente(posibles_manos_op):
    valor=0
    mejor_mano_op=posibles_manos_op[0]
    for hand in posibles_manos_op:
        if evaluate_hand(hand)>=valor:
            mejor_mano_op=hand
            valor=evaluate_hand(hand)
    print("\nLa mejor mano que puede jugar tu oponente es ", mejor_mano_op, "porque es ", evaluate_type(mejor_mano_op))
    if (evaluate_hand(mejor_mano_op))>(evaluar_mejor_valor(current_player, posibles_manos)):
        return 0.5
    else:
        return 1

def duplicar_apuesta(current_player):
    mejor_mano_pos = (evaluar_oponente(posibles_manos_op))*(float(apuesta_oponente)-current_player.apuesta)/current_player.apuesta
    
    print(mejor_mano_pos)
    if mejor_mano_pos < 0.5:
        current_player.apuesta = 2*current_player.apuesta
        print("Debes duplicar tu apuesta a ", current_player.apuesta)
    else:
        print("Debes mantener tu apuesta en ", current_player.apuesta)
    
duplicar_apuesta(current_player)


#Comprobamos
mano_op = input("Introduce la mano de tu oponente completa (a cinco cartas)")
if (evaluate_hand(mano_op))>(evaluar_mejor_valor(current_player, posibles_manos)):
   print("Perdiste")
else:
    print("¡Ganaste!")
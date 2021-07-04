:- dynamic here/1.
:- dynamic location/2.
:- dynamic have/1.
:- dynamic teeth/1.

% AVALIABLE PLACES 
place(X) :-
    room(X).
place(X) :-
    outside(X).

outside(shop).

% AVALIABLE ROOMS
room(bedroom).
room(hall).
room(bathroom).
room(kitchen).
room(cellar).

% BEDROOM
location(bed, bedroom). % give stuff weight to make it impossible to lift
location(bookCase, bedroom). % make items into objects, add weight and add max pickable weight
location(book, bookCase).
location(desk, bedroom).
location(chair, bedroom).
location(computer, desk).
location(alcohol, desk). 
location(cellphone, desk). 

% HALL
location(wardrobe, hall).
location(jacket, wardrobe).
location(closet, hall).
location(box, closet).
location(purse, box).
location(key, purse).

%BATHROOM
location(shower, bathroom).
location(showergel, shower).
location(sink, bathroom).
location(soap, sink).
location(toilet, bathroom).
location(locker, bathroom).
location(mirror, locker).
location(pills, locker).
location(toothpaste, locker).

%KITCHEN
location(oven, kitchen).
location(fridge, kitchen).
location(worktop, kitchen).
location(drawers, kitchen).
location(cupboard, kitchen).

door(bedroom, hall).
door(bathroom, hall).
door(bedroom, bathroom).
door(hall, kitchen).
door(hall, outside).

here(bedroom).

have(chewinggum). 

teeth(dirty). 



% RULES
connected(X,Y):-    % double sided connection, recursive
    door(X,Y).
connected(X,Y):-
    door(Y,X).
connected(X,Y):-
    connected(X,Z),
    connected(Z,Y),!.

isInside(Item,Location):-        % to search all items, not only visible    
  location(Item,Location).        
isInside(Item,Location):-
  location(AnyItem, Location),
  isInside(Item, AnyItem).

% COMMANDS =================================================================================
here:-
    here(Here),
    write('You are in the '), write(Here), write('.'), nl.
% ===
have:-
  have(_),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions, !.
have:-
  write('Empty hands. '),nl.

list_possessions:-
  have(X),
  write(X),nl,
  fail.
list_possessions.

% ===
list_things :-
    here(Here),
    isInside(Item, Here),
    write(Item),
    nl,
    fail.
list_things :-
    nl.
    
% ===

search :-
    write('You can see:'), nl,
    list_things.
% ===
go_to(Place) :-
    here(Place),
    write('You are already in the '),write(Place), write('. '), 
    !.
go_to(Place) :-
    room(Place),
    goToRoom(Place), 
    here,
    !.
go_to(Place) :-
    room(Place),
    write('Cant go to this room.'),
    !.
go_to(shop) :-
    outside(shop),
    goOutside(shop),
    buyToothbrush, !.
go_to(shop) :-
    outside(shop),
    write('Cant go outside. '), !.
go_to(_) :-
    write('There is no such place in this game. '),
    !.

buyToothbrush :-
    asserta(have(toothbrush)),
    write('You bought a toothbrush. Woohoo, victory is close! '),nl,
    !.

goOutside(_) :- 
    have(key),
    have(jacket), !.
goOutside(_) :-
    have(key),
    not(have(jacket)),
    write('Cant go out without a jacket. Its cold. '), fail. 
goOutside(_) :-
    not(have(key)),
    write('Find the outside door key first. '), fail. 
    
goToRoom(Room) :-
    canGoToRoom(Room),
    changeRoomTo(Room).

canGoToRoom(Room) :-
    here(Here),
    connected(Here, Room), 
    !.
canGoToRoom(Room) :-
    here(Room),
    write('You are already in the '), write(Room), write('. '), 
    !.
canGoToRoom(Room) :-
    here(Here),
    write('There is no connection from '), write(Here), write(' to the '), write(Room), write('. '),
    !.

changeRoomTo(Room):-
    retract(here(_)),
    asserta(here(Room)).

% ===



take(Item) :-
    canTake(Item),
    retract(location(Item, _)),
    asserta(have(Item)),
    write('Picked up: '), write(Item), nl, !.
take(Item) :-
    write('Item '), write(Item), write(' is not here'), nl, !.

canTake(Item) :-
    here(Here),
    isInside(Item, Here).
    

put(Item) :-
    retract(have(Item)),
    here(Room),
    asserta(location(Item, Room)).
% ===

brush_teeth :-
    here(bathroom),
    have(toothbrush), 
    have(toothpaste),
    retract(teeth(_)), 
    asserta(teeth(clean)),
    write('Your teeth are now clean. You can go to bed. '), nl, !.
brush_teeth :-
    here(bathroom),
    not(have(toothbrush)),
    write('Find a toothbrush. '), nl, !.
brush_teeth :-
    here(bathroom),
    have(toothbrush), 
    not(have(toothpaste)),
    write('Take toothpaste. '), nl, !.
brush_teeth :-
    not(here(bathroom)),
    write('Cant do it here. Go to bathroom. '), nl, !.
% ===

go_to_sleep :-
    teeth(clean),
    here(bedroom), 
    not(have(jacket)), 
    write('Mission completed. SweET dREamsss... '), nl, nl,
    write('BARDZO ŁADNIE, 3+'), nl, !. 
go_to_sleep :-
    not(teeth(clean)),
    write('You NEED TO clean your teEth... '), nl, !. 
go_to_sleep :-
    not(here(bedroom)),
    write('Go to bedroom. '), nl, !. 
go_to_sleep :-
    here(bedroom), 
    have(jacket), 
    write('Sleeping in the jacket? Take it off. '), nl, !.


% here, go_to(bathroom), search, go_to(hall), search, take(key), take(jacket), go_to(shop), go_to(bathroom), take(toothpaste), brush_teeth, go_to(bedroom), go_to_sleep.
% Where does the zebra live?
% Puzzle solution written by Claude Sammut.
top:-
	houses(Houses),
	member(house(red, english, _, _, _), Houses),
	member(house(_, spanish, dog, _, _), Houses),
	member(house(green, _, _, coffee, _), Houses),
	member(house(_, ukrainian, _, tea, _), Houses),
	right_of(house(green,_,_,_,_), house(ivory,_,_,_,_), Houses),
	member(house(_, _, snails, _, winstons), Houses),
	member(house(yellow, _, _, _, kools), Houses),
	Houses = [_, _, house(_, _, _, milk, _), _,_],
	Houses = [house(_, norwegian, _, _, _)|_],
	next_to(house(_,_,_,_,chesterfields), house(_,_,fox,_,_), Houses),
	next_to(house(_,_,_,_,kools), house(_,_,horse,_,_), Houses),
	member(house(_, _, _, orange_juice, lucky_strikes), Houses),
	member(house(_, japanese, _, _, parliaments), Houses),
	next_to(house(_,norwegian,_,_,_), house(blue,_,_,_,_), Houses),
	member(house(_, _, zebra, _, _), Houses),
	member(house(_, _, _, water, _), Houses).
%	print_houses(Houses).

houses([
	house(_, _, _, _, _),
	house(_, _, _, _, _),
	house(_, _, _, _, _),
	house(_, _, _, _, _),
	house(_, _, _, _, _)
]).

right_of(A, B, [B, A | _]).
right_of(A, B, [_ | Y]) :- right_of(A, B, Y).

next_to(A, B, [A, B | _]).
next_to(A, B, [B, A | _]).
next_to(A, B, [_ | Y]) :- next_to(A, B, Y).

member(X, [X|_]).
member(X, [_|Y]) :- member(X, Y).

print_houses([A|B]) :- !,
	write(A), nl,
	print_houses(B).
print_houses([]).

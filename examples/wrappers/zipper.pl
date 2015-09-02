% Paulo Moura
% April 30, 2013
% Public domain code

% zipper(Position, List, Zip, Element)
%
% where Zip = zip(Before, Element, After)
%
% index starts at 1

zipper(Position, List, Zip, Element) :-
	zipper(Position, List, [], Zip, Element).

zipper(1, [Head| Tail], Acc, zip(Acc, Head, Tail), Head).
zipper(N, [Head| Tail], Acc, zip(Before, Element, After), Element) :-
	N > 1,
	M is N - 1,
	zipper(M, Tail, [Head| Acc], zip(Before, Element, After), Element).

next(zip(Before, Element, [Head| Tail]), zip([Element| Before], Head, Tail)).

previous(X, Y) :- next(Y, X).

/*
?- [zipper].
% zipper compiled 0,00 sec, 2 clauses
true.

?- zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next).
Zip = zip([2, 1], 3, [4, 5]),
X = 3,
Next = zip([3, 2, 1], 4, [5]) .

?- zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next), previous(Next, Zip).
Zip = zip([2, 1], 3, [4, 5]),
X = 3,
Next = zip([3, 2, 1], 4, [5]) .

?- zipper(3, [1,2,3,4,5], Zip, X), previous(Zip, Previous).
Zip = zip([2, 1], 3, [4, 5]),
X = 3,
Previous = zip([1], 2, [3, 4, 5]) .

?- zipper(3, [1,2,3,4,5], Three, X), next(Three, Four), next(Four, Five), previous(Five, Four), previous(Four, Three), previous(Three, Two), previous(Two, One).
Three = zip([2, 1], 3, [4, 5]),
X = 3,
Four = zip([3, 2, 1], 4, [5]),
Five = zip([4, 3, 2, 1], 5, []),
Two = zip([1], 2, [3, 4, 5]),
One = zip([], 1, [2, 3, 4, 5]) .
*/                        

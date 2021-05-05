
:- initialization(set_logtalk_flag(events, allow)).


:- object(activation(_X_, _Y_, _Z_),
	implements(monitoring)).

	:- initialization(define_events(after, activation(_,_,_), _, _, activation(_,_,_))).

	:- public([a/1, b/1, c/1]).

	a(_X_).
	b(_Y_).
	c(_Z_).

	node(T) :-
		threshold(T),
		(	T =< 0 ->
			write(_X_-_Y_-_Z_), nl
		;	true
		).

	threshold(T) :-
		(nonvar(_X_) -> X is -1; X is 0),
		(nonvar(_Y_) -> Y is +1; Y is 0),
		(nonvar(_Z_) -> Z is -1; Z is 0),
		T is X + Y + Z + 1.

	after(activation(_X_, _Y_, _Z_), _, user) :-
		node(T),
		write('T = '), write(T), nl.

:- end_object.


end_of_file.


?- activation(_,_,_)::(a(a) ,b(b), c(c)).
a-_2462-_2484
T = 0
T = 1
a-b-c
T = 0
true.

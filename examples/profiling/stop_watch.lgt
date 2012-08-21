
:- object(stop_watch,
	implements(monitoring),
	imports(monitor)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/10/20,
		comment is 'Message executing time monitor.']).

	:- uses(time, [cpu_time/1]).

	before(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('STARTING at '),
		cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

	after(Object, Message, Sender) :-
		write(Object), write(' <-- '), writeq(Message),
		write(' from '), write(Sender), nl, write('ENDING at '),
		cpu_time(Seconds), write(Seconds), write(' seconds'), nl.

:- end_object.

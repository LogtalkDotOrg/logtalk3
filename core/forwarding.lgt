
:- protocol(forwarding).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/01/30,
		comment is 'Message forwarding protocol.']).

	:- public(forward/1).
	:- mode(forward(@callable), zero_or_more).
	:- info(forward/1, [
		comment is 'User-defined message forward handler, automatically called (if defined) for any message that the receiving object does not understand.',
		argnames is ['Message']]).

:- end_protocol.

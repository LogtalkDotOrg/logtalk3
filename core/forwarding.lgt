
:- protocol(forwarding).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/01/30,
		comment is 'Message forwarding protocol. The message forward handler is automatically called for any message that the receiving object does not understand.']).

	:- public(forward/1).
	:- mode(forward(@callable), zero_or_more).
	:- info(forward/1, [
		comment is 'User-defined message forward handler.',
		argnames is ['Message']]).

:- end_protocol.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(enigma).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/01/22,
		comment is 'Example of using DCG rules to decrypt a enigma where words are made of numbers corresponding to the characters on a cellphone keypad.'
	]).

	:- public(solve/2).
	:- mode(solve(+string, -list(atom)), zero_or_one).
	:- info(solve/2, [
		comment is 'Solves a cellphone enigma against a dictionary of words.',
		argnames is ['Enigma', 'Message']
	]).

	solve(Enigma, Message) :-
		phrase(message(Message), Enigma).

	message([Word| Words]) --> separator, word(Chars), {atom_chars(Word, Chars), dictionary(Word)}, !, message(Words).
	message([]) --> separator.

	word([Char| Chars]) --> character(Char), word(Chars).
	word([]) --> [].

	separator --> " ", !, separator.
	separator --> [].

	character(a) --> "2".
	character(b) --> "2".
	character(c) --> "2".

	character(d) --> "3".
	character(e) --> "3".
	character(f) --> "3".

	character(g) --> "4".
	character(h) --> "4".
	character(i) --> "4".

	character(j) --> "5".
	character(k) --> "5".
	character(l) --> "5".

	character(m) --> "6".
	character(n) --> "6".
	character(o) --> "6".

	character(p) --> "7".
	character(q) --> "7".
	character(r) --> "7".
	character(s) --> "7".

	character(t) --> "8".
	character(u) --> "8".
	character(v) --> "8".

	character(w) --> "9".
	character(x) --> "9".
	character(y) --> "9".
	character(z) --> "9".

	dictionary(dinner).
	dictionary(have).
	dictionary(i).
	dictionary(love).
	dictionary(miss).
	dictionary(much).
	dictionary(me).
	dictionary(you).
	dictionary(so).
	dictionary(to).
	dictionary(tonight).
	dictionary(with).
	dictionary(would).

:- end_object.

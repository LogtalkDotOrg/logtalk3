%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(morse).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2013/11/04,
		comment is 'Morse code decoder.'
	]).

	:- public(morse//1).
	:- mode(morse(-list(atom)), zero_or_one).
	:- info(morse//1, [
		comment is 'Recognizes a message in Morse code, returning the corresponding list of words.',
		argnames is ['Words']
	]).

	morse([Word| Words]) --> word(Characters), {atom_chars(Word, Characters)}, "  ", morse(Words).
	morse([Word]) --> word(Characters), {atom_chars(Word, Characters)}.

	word([Character| Characters]) --> symbols(Symbols), {atom_chars(Atom, Symbols), character(Atom, Character)}, " ", word(Characters).
	word([Character]) --> symbols(Symbols), {atom_chars(Atom, Symbols), character(Atom, Character)}.

	symbols([Symbol| Symbols]) --> symbol(Symbol), symbols(Symbols).
	symbols([Symbol]) --> symbol(Symbol).

	symbol('.') --> ".".
	symbol('-') --> "-".

	character('.-',   a).
	character('-...', b).
	character('-.-.', c).
	character('-..',  d).
	character('.',    e).
	character('..-.', f).
	character('--.',  g).
	character('....', h).
	character('..',   i).
	character('.---', j).
	character('-.-',  k).
	character('.-..', l).
	character('--',   m).
	character('-.',   n).
	character('---',  o).
	character('.--.', p).
	character('--.-', q).
	character('.-.',  r).
	character('...',  s).
	character('-',    t).
	character('..-',  u).
	character('...-', v).
	character('.--',  w).
	character('-..-', x).
	character('-.--', y).
	character('--..', z).

	character('.----', '1').
	character('..---', '2').
	character('...--', '3').
	character('....-', '4').
	character('.....', '5').
	character('-....', '6').
	character('--...', '7').
	character('---..', '8').
	character('----.', '9').
	character('-----', '0').

	character('.-.-.-',  '.').
	character('--..--',  ',').
	character('..--..',  '?').
	character('.----.', '''').
	character('-.-.--',  '!').
%	character('— — — ·', '!').
	character('-..-.',   '/').
	character('-.--.',   '(').
	character('-.--.-',  ')').
	character('.-...',   '&').
	character('---...',  ':').
	character('-.-.-.',  ';').
	character('-...-',   '=').
	character('.-.-.',   '+').
	character('-....-',  '-').
	character('..--.-',  '_').
	character('.-..-.',  '"').
	character('...-..-', '$').
	character('.--.-',   '@').

:- end_object.

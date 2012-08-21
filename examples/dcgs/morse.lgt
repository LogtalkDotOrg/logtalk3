%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(morse).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/06/14,
		comment is 'Morse code decoder.']).

	:- public(morse//1).
	:- mode(morse(-list(atom)), zero_or_one).
	:- info(morse//1, [
		comment is 'Recognizes a message in Morse code, returning the corresponding list of words.',
		argnames is ['Words']]).

	morse([Word| Words]) --> word(Characters), {atom_chars(Word, Characters)}, "  ", morse(Words).
	morse([Word]) --> word(Characters), {atom_chars(Word, Characters)}.

	word([Character| Characters]) --> character(LM), {code(Character, LM)}, " ", word(Characters).
	word([Character]) --> character(Symbols), {code(Character, Symbols)}.

	character([Symbol| Symbols]) --> symbol([Symbol]), character(Symbols).
	character([Symbol]) --> symbol([Symbol]).

	symbol(".") --> ".".
	symbol("-") --> "-".

	code(a, ".-").
	code(b, "-...").
	code(c, "-.-.").
	code(d, "-..").
	code(e, ".").
	code(f, "..-.").
	code(g, "--.").
	code(h, "....").
	code(i, "..").
	code(j, ".---").
	code(k, "-.-").
	code(l, ".-..").
	code(m, "--").
	code(n, "-.").
	code(o, "---").
	code(p, ".--.").
	code(q, "--.-").
	code(r, ".-.").
	code(s, "...").
	code(t, "-").
	code(u, "..-").
	code(v, "...-").
	code(w, ".--").
	code(x, "-..-").
	code(y, "-.--").
	code(z, "--..").

	code('1', ".----").
	code('2', "..---").
	code('3', "...--").
	code('4', "....-").
	code('5', ".....").
	code('6', "-....").
	code('7', "--...").
	code('8', "---..").
	code('9', "----.").
	code('0', "-----").

	code('.', ".-.-.-").
	code(',', "--..--").
	code('?', "..--..").
	code('''', ".----.").
	code('!', "-.-.--").
	%code('!', "— — — ·").
	code('/', "-..-.").
	code('(', "-.--.").
	code(')', "-.--.-").
	code('&', ".-...").
	code(':', "---...").
	code(';', "-.-.-.").
	code('=', "-...-").
	code('+', ".-.-.").
	code('-', "-....-").
	code('_', "..--.-").
	code('"', ".-..-.").
	code('$', "...-..-").
	code('@', ".--.-").

:- end_object.

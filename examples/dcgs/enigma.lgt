%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
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

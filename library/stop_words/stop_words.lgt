%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(stop_words(_Representation_, _Language_),
	implements(stop_words_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-05,
		comment is 'Stop-word enumeration and filtering for a text representation and language provider.',
		parameters is [
			'Representation' - 'Word representation. Valid values are ``atom``, ``codes``, and ``chars``.',
			'Language' - 'Object implementing the ``stop_words_language_protocol`` protocol.'
		],
		see_also is [stop_words_protocol, stop_words_language_protocol]
	]).

	stop_word(Word) :-
		(	var(Word) ->
			_Language_::stop_word(Atom),
			atom_to_word(_Representation_, Atom, Word)
		;	atom_to_word(_Representation_, Atom, Word),
			_Language_::stop_word(Atom)
		).

	is_stop_word(Word) :-
		string(_Representation_)::string_lower(Word, LowercaseWord),
		word_to_atom(_Representation_, LowercaseWord, Atom),
		_Language_::stop_word(Atom).

	exclude([], []).
	exclude([Word| Words], Filtered) :-
		(	is_stop_word(Word) ->
			exclude(Words, Filtered)
		;	Filtered = [Word| Rest],
			exclude(Words, Rest)
		).

	atom_to_word(atom, Atom, Atom).
	atom_to_word(chars, Atom, Chars) :-
		atom_chars(Atom, Chars).
	atom_to_word(codes, Atom, Codes) :-
		atom_codes(Atom, Codes).

	word_to_atom(atom, Atom, Atom).
	word_to_atom(chars, Chars, Atom) :-
		atom_chars(Atom, Chars).
	word_to_atom(codes, Codes, Atom) :-
		atom_codes(Atom, Codes).

:- end_object.

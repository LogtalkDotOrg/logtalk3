%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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


:- category(dead_code_scanner_messages).

	:- info([
		version is 0:7:0,
		author is 'Barry Evans and Paulo Moura',
		date is 2020-02-05,
		comment is 'Logtalk ``dead_code_scanner`` tool default message translations.'
	]).
	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, dead_code_scanner, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, dead_code_scanner) -->
		{ground_term_copy(Message, GroundMessage)},
		message_tokens(GroundMessage).

	message_tokens(scan_started) -->
		[].

	message_tokens(scan_ended) -->
		[].

	message_tokens(scanning_for_dead_code) -->
		['Scanning for dead code ...'-[], nl].

	message_tokens(completed_scanning_for_dead_code) -->
		['... completed scanning for dead code'-[], nl].

	message_tokens(scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Type, Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		['~w scan started at ~w-~w-~w, ~w:~w:~w'-Args, nl].

	message_tokens(scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)) -->
		{	integer_to_padded_atom(Month, MonthAtom),
			integer_to_padded_atom(Day, DayAtom),
			integer_to_padded_atom(Hours, HoursAtom),
			integer_to_padded_atom(Minutes, MinutesAtom),
			integer_to_padded_atom(Seconds, SecondsAtom),
			Args = [Type, Year, MonthAtom, DayAtom, HoursAtom, MinutesAtom, SecondsAtom]
		},
		['~w scan ended at ~w-~w-~w, ~w:~w:~w'-Args, nl].

	message_tokens(scanning_directory(Directory)) -->
		['Scanning directory ~w ...'-[Directory], nl].

	message_tokens(scanning_file(File)) -->
		['Scanning file ~w ...'-[File], nl].

	message_tokens(scanning_entity(Kind, Entity)) -->
		['Scanning ~q ~w ...'-[Entity, Kind], nl].

	message_tokens(dead_predicate(_Entity, Predicate, File, Line)) -->
		likely_dead_predicate(Predicate),
		['  in file ~w at or above line ~d'-[File, Line], nl].

	message_tokens(unknown(library, Library)) -->
		['Library not defined: ~q'-[Library], nl].
	message_tokens(unknown(directory, Directory)) -->
		['Directory does not exist: ~q'-[Directory], nl].
	message_tokens(unknown(entity, Entity)) -->
		['Entity not loaded: ~q'-[Entity], nl].

	likely_dead_predicate(Object::Functor/Arity) -->
		['Likely unused predicate: ~q'-[Object::Functor/Arity], nl].
	likely_dead_predicate(Object::Functor//Arity) -->
		['Likely unused non-terminal: ~q'-[Object::Functor//Arity], nl].
	likely_dead_predicate(':'(Module,Functor/Arity)) -->
		['Likely unused predicate: ~q'-[':'(Module,Functor/Arity)], nl].
	likely_dead_predicate(':'(Module,Functor//Arity)) -->
		['Likely unused non-terminal: ~q'-[':'(Module,Functor//Arity)], nl].
	likely_dead_predicate(Functor/Arity) -->
		['Likely dead predicate: ~q'-[Functor/Arity], nl].
	likely_dead_predicate(Functor//Arity) -->
		['Likely dead non-terminal: ~q'-[Functor//Arity], nl].

	% auxiliary predicates

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

	integer_to_padded_atom(Integer, Atom) :-
		number_codes(Integer, Codes),
		(	Integer < 10 ->
			char_code('0', ZeroCode),
			atom_codes(Atom, [ZeroCode| Codes])
		;	atom_codes(Atom, Codes)
		).

:- end_category.

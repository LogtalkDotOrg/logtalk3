%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/06/01,
		comment is 'Unit tests for the ISO Prolog standard char_code/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.6.4

	succeeds(iso_char_code_2_01) :-
		{char_code(a, Code)},
		Code == 0'a.

	succeeds(iso_char_code_2_02) :-
		{char_code(Char, 99), atom_codes(Atom, [99])},
		Char == Atom.

	succeeds(iso_char_code_2_03) :-
		{char_code(Char, 0'c)},
		Char == c.

	succeeds(iso_char_code_2_04) :-
		% the ISO standard also allows a representation_error(character_code)
		{char_code(_Char, 163)}.

	succeeds(iso_char_code_2_05) :-
		{char_code(b, 0'b)}.

	throws(iso_char_code_2_06, error(type_error(character,'ab'),_)) :-
		{char_code('ab', _Code)}.

	throws(iso_char_code_2_07, error(instantiation_error,_)) :-
		{char_code(_Char, _Code)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_char_code_2_08, error(type_error(integer,x),_)) :-
		{char_code(a, x)}.

	throws(eddbali_char_code_2_09, error(representation_error(character_code),_)) :-
		{char_code(_Char, -2)}.

	% tests from the Logtalk portability work

	succeeds(lgt_char_code_2_10) :-
		catch({char_code(_, 0)}, Error, Error = error(representation_error(character_code),_)).

:- end_object.

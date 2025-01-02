%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(default_atom_mutations).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-24,
		comment is 'Default atom mutations.',
		see_also is [type]
	]).

	:- uses(list, [
		length/2, nth1/4
	]).

	:- uses(fast_random, [
		permutation/2, random/1, select/3, select/4, swap/2, swap_consecutive/2
	]).

	% deletion of a random character
	mutation(atom, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		select(_Random, Codes, Rest),
		atom_codes(Mutation, Rest).
	% adding a random character
	mutation(atom, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		length(Codes, Length),
		random(Float),
		Index is truncate(Float*Length+1),
		type::arbitrary(code(ascii_identifier), New),
		nth1(Index, MutationCodes, New, Codes),
		atom_codes(Mutation, MutationCodes).
	% replacing a random character
	mutation(atom, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		type::arbitrary(code(ascii_identifier), New),
		select(_Random, Codes, New, MutationCodes),
		atom_codes(Mutation, MutationCodes).
	% swap two consecutive characters
	mutation(atom, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		swap_consecutive(Codes, MutationCodes),
		atom_codes(Mutation, MutationCodes).
	% swap two characters
	mutation(atom, Atom, Mutation) :-
		atom_codes(Atom, Codes),
		swap(Codes, MutationCodes),
		atom_codes(Mutation, MutationCodes).
	% permutation of the atom characters
	mutation(atom, Atom, Mutation) :-
		atom_length(Atom, Length),
		Length > 1,
		atom_codes(Atom, Codes),
		permutation(Codes, PermutedCodes),
		atom_codes(Mutation, PermutedCodes),
		Atom \== Mutation.

:- end_object.

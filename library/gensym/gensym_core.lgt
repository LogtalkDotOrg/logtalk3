%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- category(gensym_core).

	:- info([
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2022-07-26,
		comment is 'Predicates for generating unique atoms. Protocol based on the ``gensym`` module of SWI-Prolog. The predicates are declared as synchronized when the library is compiled using a backend supporting threads.'
	]).

	:- public(reset_gensym/0).
	:- mode(reset_gensym, one).
	:- info(reset_gensym/0, [
		comment is 'Resets the generator counter for all bases.'
	]).

	:- public(reset_gensym/1).
	:- mode(reset_gensym(+atom), one).
	:- info(reset_gensym/1, [
		comment is 'Resets the generator counter for a given base.',
		argnames is ['Base']
	]).

	:- public(gensym/2).
	:- mode(gensym(+atom, -atom), one).
	:- info(gensym/2, [
		comment is 'Returns a new unique atom with a given base (prefix).',
		argnames is ['Base', 'Unique']
	]).

	:- private(base_/2).
	:- dynamic(base_/2).
	:- mode(base_(?atom, ?integer), zero_or_more).
	:- info(base_/2, [
		comment is 'Table of generator bases and respective counters.',
		argnames is ['Base', 'Counter']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			reset_gensym/0,
			reset_gensym/1,
			gensym/2
		]).
	:- endif.

	reset_gensym :-
		retractall(base_(_, _)).

	reset_gensym(Base) :-
		retractall(base_(Base, _)),
		asserta(base_(Base, 0)).

	gensym(Base, Unique) :-
		(	retract(base_(Base, OldInt)) ->
			NewInt is OldInt + 1
		;	NewInt is 1
		),
		asserta(base_(Base, NewInt)),
		number_codes(NewInt, NewCodes),
		atom_codes(NewAtom, NewCodes),
		atom_concat(Base, NewAtom, Unique).

:- end_category.

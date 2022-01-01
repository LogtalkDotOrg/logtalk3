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



:- object(genint).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2015-11-16,
		comment is 'Predicates for generating integers.'
	]).

	:- public(reset_genint/0).
	:- synchronized(reset_genint/0).
	:- mode(reset_genint, one).
	:- info(reset_genint/0, [
		comment is 'Resets the generator counter for all bases.'
	]).

	:- public(reset_genint/1).
	:- synchronized(reset_genint/1).
	:- mode(reset_genint(+atom), one).
	:- info(reset_genint/1, [
		comment is 'Resets the generator counter for a given base.',
		argnames is ['Base']
	]).

	:- public(genint/2).
	:- synchronized(genint/2).
	:- mode(genint(+atom, -atom), one).
	:- info(genint/2, [
		comment is 'Returns a new unique atom for the given counter.',
		argnames is ['Base', 'Unique']
	]).

	:- private(base_/2).
	:- dynamic(base_/2).
	:- mode(base_(?atom, ?integer), zero_or_more).
	:- info(base_/2, [
		comment is 'Table of generator bases and respective counters.',
		argnames is ['Base', 'Counter']
	]).

	reset_genint :-
		retract(base_(Base, _)),
		asserta(base_(Base, 0)),
		fail.
	reset_genint.

	reset_genint(Base) :-
		retractall(base_(Base, _)),
		asserta(base_(Base, 0)).

	genint(Base, Unique) :-
		(	retract(base_(Base, OldInt)) ->
			NewInt is OldInt + 1
		;	NewInt is 1
		),
		asserta(base_(Base, NewInt)),
		number_codes(NewInt, NewCodes),
		atom_codes(NewAtom, NewCodes),
		atom_concat(Base, NewAtom, Unique).

:- end_object.

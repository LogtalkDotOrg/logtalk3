%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(persistency).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2017/10/25,
		comment is 'Simple example of object dynamic state persistency across loads.'
	]).

	:- include(persistency('state.pl')).

	:- public(state/1).
	:- mode(state(?term), zero_or_more).
	:- info(state/1, [
		comment is 'Enumerates terms in the saved state.',
		argnames is ['Term']
	]).

	:- public(add/1).
	:- mode(add(?term), zero_or_more).
	:- info(add/1, [
		comment is 'Adds a new term to the saved state.',
		argnames is ['Term']
	]).

	:- public(save/0).
	:- mode(save, one).
	:- info(save/0, [
		comment is 'Saves the dynamic state.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets the dynamic state.'
	]).

	:- private(state_/1).
	:- dynamic(state_/1).
	:- mode(state_(?term), zero_or_one).
	:- info(state_/1, [
		comment is 'Saved state term storage.',
		argnames is ['State']
	]).

	state(State) :-
		state_(State).

	add(State) :-
		callable(State),
		assertz(state_(State)).

	save :-
		logtalk::expand_library_path(persistency('state.pl'), Path),
		open(Path, write, Stream),
		forall(
			state_(State),
			(writeq(Stream, state_(State)), write(Stream, '.\n'))
		),
		close(Stream).

	reset :-
		retractall(state_(_)),
		logtalk::expand_library_path(persistency('state.pl'), Path),
		catch(ignore(os::delete_file(Path)), _, true),
		open(Path, write, Stream),
		close(Stream).

:- end_object.

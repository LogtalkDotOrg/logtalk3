%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2018-09-16,
		comment is 'Unit tests for the "patching" example.'
	]).

	cover(broken).
	cover(metaclass).
	cover(broken_class).
	cover(instance).
	cover(patch).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	succeeds(patching_01) :-
		setof(Object, complements_object(patch, Object), Objects),
		Objects == [broken, broken_class].

	succeeds(patching_02) :-
		broken::is_proper_list([1,2,3]).

	succeeds(patching_03) :-
		instance::is_proper_list([1,2,3]).

	fails(patching_04) :-
		broken::is_proper_list(_).

	fails(patching_05) :-
		instance::is_proper_list(_).

	fails(patching_06) :-
		broken::is_proper_list([a,b,c|_]).

	fails(patching_07) :-
		instance::is_proper_list([a,b,c|_]).

	throws(patching_08, error(permission_error(access, private_predicate, last/3), _)) :- 
		broken::last(_, _, _).

	throws(patching_09, error(permission_error(access, private_predicate, last/3), _)) :- 
		instance::last(_, _, _).

	succeeds(patching_10) :-
		broken::nextto(2, 3, [1,2,3]).

:- end_object.

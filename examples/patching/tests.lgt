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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/08/22,
		comment is 'Unit tests for the "patching" example.'
	]).

	:- uses(list, [memberchk/2]).

	cover(broken).
	cover(metaclass).
	cover(broken_class).
	cover(instance).
	cover(patch).

	succeeds(patching_1) :-
		setof((Category, Object), complements_object(Category, Object), Pairs),
		ground(Pairs), memberchk((patch,broken), Pairs), memberchk((patch,broken_class), Pairs).

	succeeds(patching_2) :-
		broken::is_proper_list([1,2,3]).

	succeeds(patching_3) :-
		instance::is_proper_list([1,2,3]).

	fails(patching_4) :-
		broken::is_proper_list(_).

	fails(patching_5) :-
		instance::is_proper_list(_).

	fails(patching_6) :-
		broken::is_proper_list([a,b,c|_]).

	fails(patching_7) :-
		instance::is_proper_list([a,b,c|_]).

	throws(patching_8, error(permission_error(access, private_predicate, last/3), _)) :- 
		broken::last(_, _, _).

	throws(patching_9, error(permission_error(access, private_predicate, last/3), _)) :- 
		instance::last(_, _, _).

:- end_object.

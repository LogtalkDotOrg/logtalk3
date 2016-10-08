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
		version is 1.3,
		author is 'Parker Jones and Paulo Moura',
		date is 2014/02/04,
		comment is 'Unit tests for the "multifile" example.'
	]).

	cover(main).
	cover(other).
	cover(more).

	test(multifile_1) :-
		findall(X, main::a(X), Solutions),
		Solutions == [1, 2, 3, 4, 5].

	test(multifile_2) :-
		main::current_predicate(a/1),
		main::predicate_property(a(_), public),
		main::predicate_property(a(_), multifile),
		main::predicate_property(a(_), number_of_clauses(5)).

	test(multifile_3) :-
		findall(X, main::b(X), Solutions),
		Solutions == [one, two, three].

	test(multifile_4) :-
		main::current_predicate(b/1),
		main::predicate_property(b(_), public),
		main::predicate_property(b(_), multifile),
		main::predicate_property(b(_), number_of_clauses(2)).

:- end_object.

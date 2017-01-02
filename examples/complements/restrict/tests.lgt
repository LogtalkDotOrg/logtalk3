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
		date is 2014/10/08,
		comment is 'Unit tests for the "complements/restrict" example.'
	]).

	cover(vault).
	cover(my_vault).
	cover(hacker).

	test(complements_restrict_1) :-
		complements_object(Category, Object),
		Category == hacker, Object == my_vault.

	test(complements_restrict_2) :-
		conforms_to_protocol(my_vault, Protocol),
		Protocol == monitoring.

	test(complements_restrict_3) :-
		conforms_to_protocol(my_vault, Protocol, Scope),
		Protocol == monitoring,
		Scope == (public).

	test(complements_restrict_4) :-
		my_vault::open('!"#$%&/()=').

	test(complements_restrict_5) :-
		\+ my_vault::open('1234567890').

:- end_object.

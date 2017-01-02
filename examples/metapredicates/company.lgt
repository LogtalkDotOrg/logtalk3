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


:- object(company).

	:- info([
		version is 1.0,
		date is 2010/12/20,
		author is 'Ralf Lammel; adapted to Logtalk by Paulo Moura.',
		comment is 'Example of using the map_reduce/5 meta-predicate.',
		source is 'Example adapted from the paper "Scrap Your Boilerplate-Prologically!", PPDP 2009 Invited Talk.'
	]).

	:- public([
		company/1,
		get_salary/2,
		cut_salary/2
	]).

	company([
		topdept(name('Human Resources'), manager(name('Lisa'), salary(123456)), []),
		topdept(name('Development'), manager(name('Anders'), salary(43210)), [
			subdept(name('Visual Basic'), manager(name('Amanda'), salary(8888)), []),
			subdept(name('Visual C#'), manager(name('Erik'), salary(4444)), [])
		])
	]).

	:- uses(meta, [map/3, map_reduce/5]).
	:- uses(integer, [plus/3::add/3]).

	get_salary(company(L), S) :-
		map_reduce(get_salary, add, 0, L, S).
	get_salary(topdept(_, M, L), S0) :-
		get_salary(M, S1), map_reduce(get_salary, add, 0, L, S2), add(S1, S2, S0).
	get_salary(manager(_, S1), S2) :-
		get_salary(S1, S2).
	get_salary(subdept(_, M, L), S0) :-
		get_salary(M, S1), map_reduce(get_salary, add, 0, L, S2), add(S1, S2, S0).
	get_salary(employee(_, S1), S2) :-
		get_salary(S1, S2).
	get_salary(salary(S), S).

	cut_salary(company(L1), company(L2)) :-
		map(cut_salary, L1, L2).
	cut_salary(topdept(N0, M1, L1), topdept(N0, M2, L2)) :-
		cut_salary(M1, M2), map(cut_salary, L1, L2).
	cut_salary(manager(N0, S1), manager(N0, S2)) :-
		cut_salary(S1, S2).
	cut_salary(subdept(N0, M1, L1), subdept(N0, M2, L2)) :-
		cut_salary(M1, M2), map(cut_salary, L1, L2).
	cut_salary(employee(N0, S1), employee(N0, S2)) :-
		cut_salary(S1, S2).
	cut_salary(salary(S1), salary(S2)) :-
		S2 is S1 // 2.

:- end_object.

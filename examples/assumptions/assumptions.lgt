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


:- category(assumptions).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/06/26,
		comment is 'Simple implementation of ground linear and intuitionistic assumptions.'
	]).

	:- public(assumel/1).
	:- mode(assumel(+callable), one).
	:- info(assumel/1, [
		comment is 'Assume a ground fact to be used once.',
		argnames is ['Fact']
	]).

	assumel(Fact) :-
		(	assertz((Fact :- retractall(Fact)))
		;	retractall(Fact),
			!,
			fail
		).

	:- public(assumei/1).
	:- mode(assumei(+callable), one).
	:- info(assumei/1, [
		comment is 'Assume a ground fact to be used any number of times.',
		argnames is ['Fact']
	]).

	assumei(Fact) :-
		(	assertz(Fact)
		;	retract(Fact),
			!,
			fail
		).

:- end_category.

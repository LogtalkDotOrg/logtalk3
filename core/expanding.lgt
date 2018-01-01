%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- protocol(expanding).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/07/12,
		comment is 'Term and goal expansion protocol.'
	]).

	:- built_in.

	:- public(goal_expansion/2).
	:- mode(goal_expansion(+callable, -callable), zero_or_one).
	:- info(goal_expansion/2, [
		comment is 'Defines a goal expansion. Called recursively until a fixed point is reached on goals found while compiling a source file (except for goals wrapped using the {}/1 compiler bypass control construct).',
		argnames is ['Goal', 'ExpandedGoal']
	]).

	:- public(term_expansion/2).
	:- mode(term_expansion(+term, -term), zero_or_one).
	:- mode(term_expansion(+term, -list(term)), zero_or_one).
	:- info(term_expansion/2, [
		comment is 'Defines a term expansion. Called until it succeeds on all terms read while compiling a source file (except for terms skipped by using the conditional compilation directives or wrapped using the {}/1 compiler bypass control construct).',
		argnames is ['Term', 'ExpandedTerms']
	]).

:- end_protocol.

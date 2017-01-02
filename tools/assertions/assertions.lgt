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


:- set_logtalk_flag(debug, off).


:- object(assertions(_Mode),
	implements(expanding)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/04/20,
		comment is 'A simple assertions framework. Can be used as a hook object for either suppressing assertions ("production" mode) or expanding them with file context information ("debug" mode).',
		parnames is ['Mode']
	]).

	:- public(assertion/1).
	:- meta_predicate(assertion(0)).
	:- mode(assertion(@callable), one).
	:- info(assertion/1, [
		comment is 'Checks that an assertion is true. Uses the structured message printing mechanism for printing the results using a silent message for assertion success and a error message for assertion failure.',
		argnames is ['Goal']
	]).

	:- public(assertion/2).
	:- meta_predicate(assertion(*, 0)).
	:- mode(assertion(@term, @callable), one).
	:- info(assertion/2, [
		comment is 'Checks that an assertion is true. Uses the structured message printing mechanism for printing the results using a silent message for assertion success and a error message for assertion failure. The context argument can be used to pass location data.',
		argnames is ['Context', 'Goal']
	]).

	% we use the structured printing mechanism in order to allow unit tests
	% results to be intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3
	]).

	assertion(Goal) :-
		(	catch(Goal, Error, true) ->
			(	var(Error) ->
				print_message(silent, assertions, assertion_success(Goal))
			;	print_message(error, assertions, assertion_error(Goal, Error))
			)
		;	print_message(error, assertions, assertion_failure(Goal))
		).

	assertion(Context, Goal) :-
		(	catch(Goal, Error, true) ->
			(	var(Error) ->
				print_message(silent, assertions, assertion_success(Context, Goal))
			;	print_message(error, assertions, assertion_error(Context, Goal, Error))
			)
		;	print_message(error, assertions, assertion_failure(Context, Goal))
		).

	% the following clauses for the goal_expansion/2 predicate are only used when
	% this object is used as a hook object with its parameter instantiated
	goal_expansion(assertion(Goal), ExpandedGoal) :-
		parameter(1, Mode),
		(	Mode == debug ->
			logtalk_load_context(source, File),
			logtalk_load_context(term_position, Position),
			ExpandedGoal = assertions::assertion(file_lines(File,Position), Goal)
		;	Mode == production ->
			ExpandedGoal = true
		;	fail
		).
	goal_expansion(assertion(_, _), true) :-
		parameter(1, Mode),
		Mode == production.

:- end_object.


:- object(assertions,
	extends(assertions(_Mode))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/04/03,
		comment is 'Proxy object for simplifying the use of the assertion meta-predicates.'
	]).

:- end_object.

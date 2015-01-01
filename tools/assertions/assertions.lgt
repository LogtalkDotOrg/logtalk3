%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2015 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

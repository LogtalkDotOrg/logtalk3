%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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


:- object(assertions).

	:- set_logtalk_flag(debug, off).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/03/27,
		comment is 'A simple assertions framework.'
	]).

	:- public(assertion/1).
	:- meta_predicate(assertion(0)).
	:- mode(assertion(@callable), zero_or_one).
	:- info(assertion/1, [
		comment is 'Checks that the assertion goal is true.',
		argnames is ['Goal']
	]).

	:- public(assertion/2).
	:- meta_predicate(assertion(*, 0)).
	:- mode(assertion(@term, @callable), zero_or_one).
	:- info(assertion/2, [
		comment is 'Checks that the assertion goal is true. The context argument allows addtional information to be passed when reporting the assertion results.',
		argnames is ['Context', 'Goal']
	]).

	% we use the structured printing mechanism in order to allow unit tests
	% results to be intercepted for alternative reporting by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3
	]).

	assertion(Goal) :-
		(	catch(Goal, Error, assertion_error_handler(Goal, Error)) ->
			print_message(silent, assertions, assertion_sucess(Goal))
		;	print_message(error, assertions, assertion_failure(Goal)),
			fail
		).

	assertion_error_handler(Goal, Error) :-
		print_message(error, assertions, assertion_error(Goal, Error)),
		throw(Error).

	assertion(Context, Goal) :-
		(	catch(Goal, Error, assertion_error_handler(Context, Goal, Error)) ->
			print_message(silent, assertions, assertion_sucess(Context, Goal))
		;	print_message(error, assertions, assertion_failure(Context, Goal)),
			fail
		).

	assertion_error_handler(Context, Goal, Error) :-
		print_message(error, assertions, assertion_error(Context, Goal, Error)),
		throw(Error).

:- end_object.

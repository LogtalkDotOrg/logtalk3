%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(expanding).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/14,
		comment is 'Term and goal expansion protocol.']).

	:- public(goal_expansion/2).
	:- dynamic(goal_expansion/2).
	:- mode(goal_expansion(+callable, -callable), zero_or_one).
	:- info(goal_expansion/2, [
		comment is 'Defines a goal expansion. This predicate is called recursively until a fixed point is reached on goals found while compiling a source file (including clause body goals, initialization goals, and conditional compilation goals).',
		argnames is ['Goal', 'ExpandedGoal']]).

	:- public(term_expansion/2).
	:- dynamic(term_expansion/2).
	:- mode(term_expansion(+term, -term), zero_or_one).
	:- mode(term_expansion(+term, -list(term)), zero_or_one).
	:- info(term_expansion/2, [
		comment is 'Defines a term expansion. This predicate is called on all terms read while compiling a source file except those skipped by using the conditional compilation directives.',
		argnames is ['Term', 'ExpandedTerms']]).

	% the remaining predicates are experimental

	:- public(goal_annotation/4).
	:- dynamic(goal_annotation/4).
	:- mode(goal_annotation(@callable, -callable, -callable, -callable), zero_or_one).
	:- info(goal_annotation/4, [
		comment is 'Defines a goal annotation. Experimental.',
		argnames is ['Annotation', 'Left', 'Right', 'Head']]).

	:- public(value_annotation/4).
	:- dynamic(value_annotation/4).
	:- mode(value_annotation(@callable, -term, -callable, -callable), zero_or_one).
	:- info(value_annotation/4, [
		comment is 'Defines a value annotation. Experimental.',
		argnames is ['Annotation', 'Value', 'Goal', 'Head']]).

	:- public(body_annotation/3).
	:- dynamic(body_annotation/3).
	:- mode(body_annotation(@callable, -callable, -callable), zero_or_one).
	:- info(body_annotation/3, [
		comment is 'Defines a body annotation. Experimental.',
		argnames is ['Annotation', 'Left', 'Right']]).

:- end_protocol.

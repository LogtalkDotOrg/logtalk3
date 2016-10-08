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


:- object(doclet).

	:- info([
		version is 0.5,
		author is 'Paulo Moura',
		date is 2016/08/03,
		comment is 'Utility object to help automate (re)generate documentation for a project.'
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	:- uses(os, [
		shell/2
	]).

	:- public(update/0).
	:- mode(update, zero_or_one).
	:- info(update/0, [
		comment is 'Updates the project documentation, first by calling a sequence of goals and second by executing a sequence of shell commands. Fails if one of the update steps fails.'
	]).

	:- public(doc_goal/1).
	:- mode(doc_goal(?callable), one_or_more).
	:- info(doc_goal/1, [
		comment is 'Table of goals, typically using the "diagrams" and the "lgtdoc" tools, used to generate the documentation. Goals are called in the order they are defined and in the context of the "user" pseudo-object.',
		argnames is ['Goal']
	]).

	:- public(shell_command/1).
	:- mode(shell_command(?atom), one_or_more).
	:- info(shell_command/1, [
		comment is 'Table of shell commands to convert intermediate documentation files into user-friendly documentation. Commands are executed in the order they are defined.',
		argnames is ['Command']
	]).

	update :-
		forall(
			::doc_goal(Goal),
			call_doc_goal(Goal)
		),
		forall(
			::shell_command(Command),
			execute_command(Command)
		).

	% silence meta-predicate warnings
	:- meta_predicate(call_doc_goal(*)).

	call_doc_goal(Goal) :-
		print_message(comment, doclet, calling_goal(Goal)),
		(	catch({Goal}, Error, true) ->
			(	var(Error) ->
				true
			;	print_message(warning, doclet, goal_error(Goal, Error)),
				fail
			)
		;	print_message(warning, doclet, goal_failure(Goal)),
			fail
		).

	execute_command(Command) :-
		print_message(comment, doclet, executing_command(Command)),
		shell(Command, Status),
		(	Status =:= 0 ->
			true
		;	print_message(warning, doclet, command_failure(Command, Status)),
			fail
		).

	% default message translations

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, doclet) -->
		message_tokens(Message).

	message_tokens(calling_goal(Goal)) -->
		['calling goal:'-[], nl, '  ~q'-[Goal], nl].

	message_tokens(goal_failure(_Goal)) -->
		['  goal failed'-[], nl].

	message_tokens(goal_error(_Goal, Error)) -->
		['  goal generated an error (~q)'-[Error], nl].

	message_tokens(executing_command(Command)) -->
		['executing command:'-[], nl, '  ~w'-[Command], nl].

	message_tokens(command_failure(_Command, Status)) -->
		['  command execution failed with status ~w'-[Status], nl].

:- end_object.


% avoid polluting SWI-Prolog meta-predicate analysis with
% "doclet" private meta-predicates
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- meta_predicate('$doclet#0.call_doc_goal#1'(*,*)).
:- endif.

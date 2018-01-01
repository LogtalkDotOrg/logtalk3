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



:- protocol(queuep).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Queue protocol.'
	]).

	:- public(empty/1).
	:- mode(empty(@queue), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the queue is empty.',
		argnames is ['Queue']
	]).

	:- public(head/2).
	:- mode(head(+queue, ?term), zero_or_one).
	:- info(head/2, [
		comment is 'Unifies Head with the first element of the queue.',
		argnames is ['Queue', 'Head']
	]).

	:- public(join/3).
	:- mode(join(@term, +queue, -queue), zero_or_one).
	:- info(join/3, [
		comment is 'Adds the new element at the end of the queue.',
		argnames is ['Element', 'Queue_in', 'Queue_out']
	]).

	:- public(join_all/3).
	:- mode(join_all(+list, +queue, -queue), zero_or_one).
	:- info(join_all/3, [
		comment is 'Adds the new elements at the end of the queue.  The elements are added in the same order that they appear in the list.',
		argnames is ['List', 'Queue_in', 'Queue_out']
	]).

	:- public(jump/3).
	:- mode(jump(@term, +queue, -queue), zero_or_one).
	:- info(jump/3, [
		comment is 'Adds the new element at the front of the queue.',
		argnames is ['Element', 'Queue_in', 'Queue_out']
	]).

	:- public(jump_all/3).
	:- mode(jump_all(+list, +queue, -queue), zero_or_one).
	:- info(jump_all/3, [
		comment is 'Adds the new elements at the front of the queue.  The elements are added in the same order that they appear in the list.',
		argnames is ['Element', 'Queue_in', 'Queue_out']
	]).

	:- public(length/2).
	:- mode(length(+queue, ?integer), zero_or_one).
	:- info(length/2, [
		comment is 'Queue length.',
		argnames is ['Queue', 'Length']
	]).

	:- public(serve/3).
	:- mode(serve(+queue, ?term, -queue), zero_or_one).
	:- info(serve/3, [
		comment is 'Removes the first element of the queue for service.',
		argnames is ['Queue_in', 'Head', 'Queue_out']
	]).

	:- public(as_list/2).
	:- mode(as_list(+queue, -list), one).
	:- info(as_list/2, [
		comment is 'Converts a queue to a list.',
		argnames is ['Queue', 'List']
	]).

:- end_protocol.

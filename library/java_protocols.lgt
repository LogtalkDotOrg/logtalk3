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


:- protocol(java_access_protocol).

	:- info([
		version is 1.01,
		author is 'Paulo Moura and Sergio Castro',
		date is 2016/09/22,
		comment is 'Protocol for a minimal abstraction for calling Java from Logtalk using familiar message sending syntax.'
	]).

	:- public(get_field/2).
	:- mode(get_field(+atom, ?nonvar), zero_or_one).
	:- info(get_field/2, [
		comment is 'Gets the value of a class or object field.',
		argnames is ['Field', 'Value']
	]).

	:- public(set_field/2).
	:- mode(set_field(+atom, +nonvar), one).
	:- info(set_field/2, [
		comment is 'Sets the value of a class or object field.',
		argnames is ['Field', 'Value']
	]).

	:- public(new/2).
	:- mode(new(+list(nonvar), -reference), one).
	:- info(new/2, [
		comment is 'Creates a new instance using the specified parameter values.',
		argnames is ['Parameters', 'Instance']
	]).

	:- public(new/1).
	:- mode(new(-reference), one).
	:- info(new/1, [
		comment is 'Creates a new instance using default parameter values.',
		argnames is ['Instance']
	]).

	:- public(invoke/1).
	:- mode(invoke(@nonvar), one).
	:- info(invoke/1, [
		comment is 'Invokes a method. This is a more efficient compared with relying on the forward/1 handler to resolve methods.',
		argnames is ['Method']
	]).

:- end_protocol.


:- protocol(java_utils_protocol).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/11/07,
		comment is 'Abstract interface to Java utility predicates.'
	]).

	:- public(value_reference/2).
	:- mode(value_reference(?atom, --var), one_or_more).
	:- info(value_reference/2, [
		comment is 'Returns an opaque term that represents the Java value.',
		argnames is ['Value', 'Reference']
	]).

	:- public(true/1).
	:- mode(true(--var), one).
	:- info(true/1, [
		comment is 'Returns an opaque term that represents the Java value true.',
		argnames is ['Reference']
	]).

	:- public(false/1).
	:- mode(false(--var), one).
	:- info(false/1, [
		comment is 'Returns an opaque term that represents the Java value false.',
		argnames is ['Reference']
	]).

	:- public(void/1).
	:- mode(void(--var), one).
	:- info(void/1, [
		comment is 'Returns an opaque term that represents the Java value void.',
		argnames is ['Reference']
	]).

	:- public(null/1).
	:- mode(null(--var), one).
	:- info(null/1, [
		comment is 'Returns an opaque term that represents the Java value null.',
		argnames is ['Reference']
	]).

	:- public(is_true/1).
	:- mode(is_true(++ground), zero_or_one).
	:- info(is_true/1, [
		comment is 'True when the argument is the Java value true. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_false/1).
	:- mode(is_false(++ground), zero_or_one).
	:- info(is_false/1, [
		comment is 'True when the argument is the Java value false. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_void/1).
	:- mode(is_void(++ground), zero_or_one).
	:- info(is_void/1, [
		comment is 'True when the argument is the Java value void. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_null/1).
	:- mode(is_null(++ground), zero_or_one).
	:- info(is_null/1, [
		comment is 'True when the argument is the Java value null. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(array_list/2).
	:- mode(array_list(+array, -list), one).
	:- mode(array_list(-array, +list), one).
	:- info(array_list/2, [
		comment is 'Converts between an array and a list.',
		argnames is ['Array', 'List']
	]).

	:- public(iterator_element/2).
	:- mode(iterator_element(+iterator, -element), zero_or_more).
	:- info(iterator_element/2, [
		comment is 'Enumerates, by backtracking, all iterator elements.',
		argnames is ['Iterator', 'Element']
	]).

:- end_protocol.

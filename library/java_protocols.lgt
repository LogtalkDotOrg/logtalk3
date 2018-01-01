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
		version is 1.5,
		author is 'Paulo Moura',
		date is 2017/10/16,
		comment is 'Abstract interface to Java utility predicates.'
	]).

	:- public(value_reference/2).
	:- mode(value_reference(?atom, --ground), one_or_more).
	:- info(value_reference/2, [
		comment is 'Returns an opaque term that represents the Java value with the given name.',
		argnames is ['Value', 'Reference']
	]).

	:- public(true/1).
	:- mode(true(--ground), one).
	:- info(true/1, [
		comment is 'Returns an opaque term that represents the Java value true.',
		argnames is ['Reference']
	]).

	:- public(false/1).
	:- mode(false(--ground), one).
	:- info(false/1, [
		comment is 'Returns an opaque term that represents the Java value false.',
		argnames is ['Reference']
	]).

	:- public(void/1).
	:- mode(void(--ground), one).
	:- info(void/1, [
		comment is 'Returns an opaque term that represents the Java value void.',
		argnames is ['Reference']
	]).

	:- public(null/1).
	:- mode(null(--ground), one).
	:- info(null/1, [
		comment is 'Returns an opaque term that represents the Java value null.',
		argnames is ['Reference']
	]).

	:- public(is_true/1).
	:- mode(is_true(@term), zero_or_one).
	:- info(is_true/1, [
		comment is 'True when the argument is the Java value true. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_false/1).
	:- mode(is_false(@term), zero_or_one).
	:- info(is_false/1, [
		comment is 'True when the argument is the Java value false. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_void/1).
	:- mode(is_void(@term), zero_or_one).
	:- info(is_void/1, [
		comment is 'True when the argument is the Java value void. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_null/1).
	:- mode(is_null(@term), zero_or_one).
	:- info(is_null/1, [
		comment is 'True when the argument is the Java value null. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(is_object/1).
	:- mode(is_object(@term), zero_or_one).
	:- info(is_object/1, [
		comment is 'True when the argument is a reference to a Java object. Fails if the argument is not instantiated.',
		argnames is ['Reference']
	]).

	:- public(terms_to_array/2).
	:- mode(terms_to_array(++list(ground), -array), one).
	:- info(terms_to_array/2, [
		comment is 'Converts a list of ground Prolog terms to an array (a Java reference).',
		argnames is ['Terms', 'Array']
	]).

	:- public(array_to_terms/3).
	:- mode(array_to_terms(+array, -list(ground), -integer), one).
	:- info(array_to_terms/3, [
		comment is 'Converts an array (a Java reference) to a list of ground Prolog terms returning also its length. The array elements must be atoms, integers, floats, or compound terms. Fails otherwise.',
		argnames is ['Array', 'Terms', 'Length']
	]).

	:- public(array_to_terms/2).
	:- mode(array_to_terms(+array, -list(term)), one).
	:- info(array_to_terms/2, [
		comment is 'Converts an array (a Java reference) to a list of ground Prolog terms. The array elements must be atoms, integers, floats, or ground compound terms. Fails otherwise.',
		argnames is ['Array', 'Terms']
	]).

	:- public(array_list/2).
	:- mode(array_list(+array, -list), one).
	:- mode(array_list(-array, +list), one).
	:- info(array_list/2, [
		comment is 'Converts between an array (a Java reference) and a list. Deprecated. Use the terms_to_array/2 and array_to_terms/2-3 instead in new code.',
		argnames is ['Array', 'List']
	]).

	:- public(iterator_element/2).
	:- mode(iterator_element(+iterator, -element), zero_or_more).
	:- info(iterator_element/2, [
		comment is 'Enumerates, by backtracking, all iterator elements.',
		argnames is ['Iterator', 'Element']
	]).

	:- public(decode_exception/2).
	:- mode(decode_exception(+callable, -atom), zero_or_one).
	:- info(decode_exception/2, [
		comment is 'Decodes an exception into its corresponding cause. Fails if the exception is not a Java exception.',
		argnames is ['Exception', 'Cause']
	]).

	:- public(decode_exception/3).
	:- mode(decode_exception(+callable, -atom, -list(atom)), zero_or_one).
	:- info(decode_exception/3, [
		comment is 'Decodes an exception into its corresponding cause and a stack trace. Fails if the exception is not a Java exception.',
		argnames is ['Exception', 'Cause', 'StackTrace']
	]).

:- end_protocol.

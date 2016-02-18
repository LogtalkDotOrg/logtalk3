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


/*
This file contains a protocol for the predicates used for logical assignment 
of Prolog terms developed by Nobukuni Kino. For more information, please 
consult the URL http://www.kprolog.com/en/logical_assignment/
*/


:- protocol(assignvarsp).

	:- info([
		version is 1.0,
		author is 'Nobukuni Kino and Paulo Moura',
		date is 2015/07/31,
		comment is 'Assignable variables (supporting backtracable assignement of non-variable terms) protocol.'
	]).

	:- public(assignable/1).
	:- mode(assignable(-assignvar), one).
	:- info(assignable/1, [
		comment is 'Makes Variable an assignable variable. Initial state will be empty.',
		argnames is ['Variable'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable')
		]
	]).

	:- public(assignable/2).
	:- mode(assignable(-assignvar, @nonvar), one).
	:- info(assignable/2, [
		comment is 'Makes Variable an assignable variable and sets its initial state to Value.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not a variable' - type_error(variable, 'Variable'),
			'Value is not instantiated' - instantiation_error
		]
	]).

	:- public((<=)/2).
	:- public(op(100, xfx, '<=')).
	:- mode(<=(?assignvar, @nonvar), one).
	:- info((<=)/2, [
		comment is 'Sets the state of the assignable variable Variable to Value (initializing the variable if needed).',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Value is not instantiated' - instantiation_error
		]
	]).

	:- public((=>)/2).
	:- public(op(100, xfx, '=>')).
	:- mode(=>(+assignvar, ?nonvar), zero_or_one).
	:- info((=>)/2, [
		comment is 'Unifies Value with the current state of the assignable variable Variable.',
		argnames is ['Variable', 'Value'],
		exceptions is [
			'Variable is not instantiated' - instantiation_error
		]
	]).

:- end_protocol.

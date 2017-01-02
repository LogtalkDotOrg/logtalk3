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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard set_prolog_flag/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.17.1.4

	succeeds(iso_set_prolog_flag_2_01) :-
		{set_prolog_flag(unknown, fail)},
		{current_prolog_flag(unknown, fail)}.

	throws(iso_set_prolog_flag_2_02, error(instantiation_error,_)) :-
		{set_prolog_flag(_X, off)}.

	throws(iso_set_prolog_flag_2_03, error(type_error(atom,5),_)) :-
		{set_prolog_flag(5, decimals)}.

	throws(iso_set_prolog_flag_2_04, error(domain_error(prolog_flag,date),_)) :-
		{set_prolog_flag(date, 'July 1999')}.

	throws(iso_set_prolog_flag_2_05, error(domain_error(flag_value,debug+trace),_)) :-
		{set_prolog_flag(debug, trace)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_set_prolog_flag_2_06, error(permission_error(modify,flag,max_arity),_)) :-
		{set_prolog_flag(max_arity, 40)}.

:- end_object.

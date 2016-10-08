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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2014/08/03,
		comment is 'Unit tests for the ISO Prolog standard op/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.14.3.4

	succeeds(iso_op_3_01) :-
		{op(30, xfy, ++)},
		{current_op(30, xfy, ++), op(0, xfy, ++)}.

	succeeds(iso_op_3_02) :-
		{op(30, xfy, ++), op(0, xfy, ++)},
		{\+ current_op(_, xfy, ++)}.

	throws(iso_op_3_03, error(type_error(integer,max),_)) :-
		{op(max, xfy, ++)}.

	throws(iso_op_3_04, error(domain_error(operator_priority,-30),_)) :-
		{op(-30, xfy, ++)}.

	throws(iso_op_3_05, error(domain_error(operator_priority,1201),_)) :-
		{op(1201, xfy, ++)}.

	throws(iso_op_3_06, error(instantiation_error,_)) :-
		{op(30, _XFY, ++)}.

	throws(iso_op_3_07, error(domain_error(operator_specifier,yfy),_)) :-
		{op(30, yfy, ++)}.

	throws(iso_op_3_08, error(type_error(list,0),_)) :-
		{op(30, xfy, 0)}.

	succeeds(iso_op_3_09) :-
		{(op(30, xfy, ++), op(40, xfx, ++))},
		{current_op(40, xfx, ++), op(0, xfx, ++)}.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == yap))).
		% these Prolog systems support the definition of an atom as both an infix and a postfix operator
		succeeds(iso_op_3_10) :-
			true.
	:- else.
		throws(iso_op_3_10, error(permission_error(create,operator,++),_)) :-
			{op(30, xfy, ++), op(50, yf, ++)}.
	:- endif.

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.14.3.4

	throws(iso_op_3_11, error(permission_error(create,operator,{}),_)) :-
		{op(500, xfy, {})}.

	throws(iso_op_3_12, error(permission_error(create,operator,{}),_)) :-
		{op(500, xfy, [{}])}.

	throws(iso_op_3_13, error(permission_error(create,operator,'|'),_)) :-
		{op(1000, xfy, '|')}.

	throws(iso_op_3_14, error(permission_error(create,operator,'|'),_)) :-
		{op(1000, xfy, ['|'])}.

	throws(iso_op_3_15, error(permission_error(create,operator,'|'),_)) :-
		{op(1150, fx, '|')}.

	succeeds(iso_op_3_16) :-
		{op(1105, xfy, '|')},
		{current_op(Priority, Specifier, '|')}, Priority == 1105, Specifier == xfy.

	succeeds(iso_op_3_17) :-
		{op(0, xfy, '|')},
		{\+ current_op(_, xfy, '|')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_op_3_18, error(instantiation_error,_)) :-
		{op(_, xfx, ++)}.

	throws(sics_op_3_19, error(instantiation_error,_)) :-
		{op(100, xfx, _)}.

	throws(sics_op_3_20, error(instantiation_error,_)) :-
		{op(100, xfx, [a|_])}.

	throws(sics_op_3_21, error(instantiation_error,_)) :-
		{op(100, xfx, [a,_])}.

	throws(sics_op_3_22, error(type_error(atom,200),_)) :-
		{op(100, 200, [a])}.

	throws(sics_op_3_23, error(type_error(atom,f(1)),_)) :-
		{op(100, f(1), [a])}.

	throws(sics_op_3_24, error(type_error(atom,a+b),_)) :-
		{op(100, xfx, [a,a+b])}.

	throws(sics_op_3_25, error(permission_error(modify,operator,(',')),_)) :-
		{op(100, xfx, (','))}.

	throws(sics_op_3_26, error(permission_error(modify,operator,(',')),_)) :-
		{op(100, xfx, [a,(',')])}.

:- end_object.

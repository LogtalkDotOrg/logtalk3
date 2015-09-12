%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		date is 2015/04/25,
		comment is 'Unit tests for the create_logtalk_flag/3 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(create_logtalk_flag_3_01, error(instantiation_error, logtalk(create_logtalk_flag(_,_,_),_))) :-
		{create_logtalk_flag(_, _, _)}.

	throws(create_logtalk_flag_3_02, error(instantiation_error, logtalk(create_logtalk_flag(_,_,_),_))) :-
		{create_logtalk_flag(a, b, [c|_])}.

	throws(create_logtalk_flag_3_03, error(instantiation_error, logtalk(create_logtalk_flag(_,_,_),_))) :-
		{create_logtalk_flag(a, b(_), [c])}.

	throws(create_logtalk_flag_3_04, error(instantiation_error, logtalk(create_logtalk_flag(_,_,_),_))) :-
		{create_logtalk_flag(a, b, [c(_)])}.

	throws(create_logtalk_flag_3_05, error(type_error(atom,1), logtalk(create_logtalk_flag(1,a,[]),_))) :-
		{create_logtalk_flag(1, a, [])}.

	throws(create_logtalk_flag_3_06, error(type_error(list,b), logtalk(create_logtalk_flag(a,1,b),_))) :-
		{create_logtalk_flag(a, 1, b)}.

	throws(create_logtalk_flag_3_07, error(domain_error(flag_option,b), logtalk(create_logtalk_flag(a,false,[b]),_))) :-
		{create_logtalk_flag(a, false, [b])}.

	throws(create_logtalk_flag_3_08, error(domain_error(flag_option,access(b)), logtalk(create_logtalk_flag(a,false,[access(b)]),_))) :-
		{create_logtalk_flag(a, false, [access(b)])}.

	throws(create_logtalk_flag_3_09, error(domain_error(flag_option,keep(b)), logtalk(create_logtalk_flag(a,false,[keep(b)]),_))) :-
		{create_logtalk_flag(a, false, [keep(b)])}.

	throws(create_logtalk_flag_3_10, error(domain_error(flag_option,type(b)), logtalk(create_logtalk_flag(a,false,[type(b)]),_))) :-
		{create_logtalk_flag(a, false, [type(b)])}.

	throws(create_logtalk_flag_3_11, error(permission_error(modify,flag,debug), logtalk(create_logtalk_flag(debug,false,[]),_))) :-
		{create_logtalk_flag(debug, false, [])}.

	throws(create_logtalk_flag_3_12, error(type_error(atom,1), logtalk(create_logtalk_flag(a,1,[type(atom)]),_))) :-
		{create_logtalk_flag(a, 1, [type(atom)])}.

	throws(create_logtalk_flag_3_13, error(permission_error(modify,flag,ro), logtalk(set_logtalk_flag(ro,false),_))) :-
		{	create_logtalk_flag(ro, true, [access(read_only)]),
			set_logtalk_flag(ro, false)
		}.

	succeeds(create_logtalk_flag_3_14) :-
		{	create_logtalk_flag(raining, true, []),
			current_logtalk_flag(raining, Value1),
			Value1 == true,
			set_logtalk_flag(raining, false),
			current_logtalk_flag(raining, Value2),
			Value2 == false
		}.

	succeeds(create_logtalk_flag_3_15) :-
		{	create_logtalk_flag(raining, true, []),
			current_logtalk_flag(raining, Value1),
			Value1 == true,
			create_logtalk_flag(raining, false, [keep(true)]),
			current_logtalk_flag(raining, Value2),
			Value2 == true
		}.

	succeeds(create_logtalk_flag_3_16) :-
		{	create_logtalk_flag(foo, bar, [type(term)]),
			current_logtalk_flag(foo, Value1),
			Value1 == bar,
			set_logtalk_flag(foo, baz),
			current_logtalk_flag(foo, Value2),
			Value2 == baz
		}.

	succeeds(create_logtalk_flag_3_17) :-
		{	create_logtalk_flag(bar, baz, [access(read_only)]),
			current_logtalk_flag(bar, Value1),
			Value1 == baz
		}.

:- end_object.

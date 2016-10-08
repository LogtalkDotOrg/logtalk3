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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard number_chars/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.7.4

	succeeds(iso_number_chars_2_01) :-
		{number_chars(33, L)},
		L == ['3','3'].

	succeeds(iso_number_chars_2_02) :-
		{number_chars(33, ['3','3'])}.

	succeeds(iso_number_chars_2_03) :-
		{number_chars(33.0, L), number_chars(N, L)},
		N == 33.0.

	succeeds(iso_number_chars_2_04) :-
		{number_chars(X, ['3','.','3','E','+','0'])},
		X == 3.3.

	succeeds(iso_number_chars_2_05) :-
		{number_chars(3.3, ['3'| _L])}.

	succeeds(iso_number_chars_2_06) :-
		{number_chars(A, ['-','2','5'])},
		A == -25.

	succeeds(iso_number_chars_2_07) :-
		{number_chars(A, ['\n',' ','3'])},
		A == 3.

	throws(iso_number_chars_2_08, error(syntax_error(_),_)) :-
		{number_chars(_A, ['3',' '])}.

	succeeds(iso_number_chars_2_09) :-
		{number_chars(A, ['0',x,f])},
		A == 15.

	succeeds(iso_number_chars_2_10) :-
		{number_chars(A, ['0','''',a])},
		A == 0'a.

	succeeds(iso_number_chars_2_11) :-
		{number_chars(A, ['4','.','2'])},
		A == 4.2.

	succeeds(iso_number_chars_2_12) :-
		{number_chars(A, ['4','2','.','0','e','-','1'])},
		A == 4.2.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_number_chars_2_13, error(instantiation_error,_)) :-
		{number_chars(_A, _L)}.

	throws(eddbali_number_chars_2_14, error(type_error(number,a),_)) :-
		{number_chars(a, _L)}.

	throws(eddbali_number_chars_2_15, error(type_error(list,4),_)) :-
		{number_chars(_A, 4)}.

	throws(eddbali_number_chars_2_16, error(type_error(character,2),_)) :-
		{number_chars(_A, ['4',2])}.

	throws(sics_number_chars_2_17, error(instantiation_error,_)) :-
		{number_chars(_A, [a|_L])}.

	throws(sics_number_chars_2_18, error(instantiation_error,_)) :-
		{number_chars(_A, [a,_L])}.

	succeeds(sics_number_chars_2_19) :-
		{number_chars(X, [' ','0','o','1','1'])},
		X == 9.

	succeeds(sics_number_chars_2_20) :-
		{number_chars(X, [' ','0','x','1','1'])},
		X == 17.

	succeeds(sics_number_chars_2_21) :-
		{number_chars(X, [' ','0','b','1','1'])},
		X == 3.

	throws(sics_number_chars_2_22, error(syntax_error(_),_)) :-
		{number_chars(_X, ['0','o','8'])}.

	throws(sics_number_chars_2_23, error(syntax_error(_),_)) :-
		{number_chars(_X, ['0','b','2'])}.

	throws(sics_number_chars_2_24, error(syntax_error(_),_)) :-
		{number_chars(_X, ['0','x','g'])}.

	% the following test is disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8
	% for all Logtalk supported backend Prolog compilers

	- throws(sics_number_chars_2_25, error(syntax_error(_),_)) :-
		{number_chars(_X, ['á'])}.

	throws(sics_number_chars_2_26, error(syntax_error(_),_)) :-
		{number_chars(_X, ['a'])}.

	throws(sics_number_chars_2_27, error(syntax_error(_),_)) :-
		{number_chars(_X, ['0','x','0','.','0'])}.

:- end_object.

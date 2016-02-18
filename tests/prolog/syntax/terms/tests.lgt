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
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog term syntax.'
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.3.1

	succeeds(iso_term_01) :-
		^^set_text_input('f(x,y). '),
		{read(_)}.

	succeeds(iso_term_02) :-
		^^set_text_input('f(:-, ;, [:-, :-|:-]). '),
		{read(_)}.

	throws(iso_term_03, error(syntax_error(_),_)) :-
		^^set_text_input('f(,,a). '),
		{read(_)}.

	throws(iso_term_04, error(syntax_error(_),_)) :-
		^^set_text_input('[a,,|v]. '),
		{read(_)}.

	throws(iso_term_05, error(syntax_error(_),_)) :-
		^^set_text_input('[a,b|,]. '),
		{read(_)}.

	succeeds(iso_term_06) :-
		^^set_text_input('f(\',\',a). '),
		{read(_)}.

	succeeds(iso_term_07) :-
		^^set_text_input('[a,\',\'|v]. '),
		{read(_)}.

	succeeds(iso_term_08) :-
		^^set_text_input('[a,b|\',\']. '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.

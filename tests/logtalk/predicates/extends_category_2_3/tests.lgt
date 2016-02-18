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
		date is 2013/03/11,
		comment is 'Unit tests for the extends_category/2-3 built-in predicates.'
	]).

	% extends_category/2 tests

	throws(extends_category_2_1, error(type_error(category_identifier, 1), logtalk(extends_category(1, _), _))) :-
		extends_category(1, _).

	throws(extends_category_2_2, error(type_error(category_identifier, 1), logtalk(extends_category(_, 1), _))) :-
		extends_category(_, 1).

	% extends_category/3 tests

	throws(extends_category_3_1, error(type_error(category_identifier, 1), logtalk(extends_category(1, _, _), _))) :-
		extends_category(1, _, _).

	throws(extends_category_3_2, error(type_error(category_identifier, 1), logtalk(extends_category(_, 1, _), _))) :-
		extends_category(_, 1, _).

	throws(extends_category_3_3, error(type_error(atom, 1), logtalk(extends_category(_, _, 1), _))) :-
		extends_category(_, _, 1).

	throws(extends_category_3_4, error(domain_error(scope, a), logtalk(extends_category(_, _, a), _))) :-
		extends_category(_, _, a).

:- end_object.

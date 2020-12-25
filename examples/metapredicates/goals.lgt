%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% the most simple meta-predicates accept goals as
% meta-arguments as illustrated in this example


:- object(simple).

	% the meta_predicate/1 directive below informs the compiler that
	% the meta-predicate calls its argument as a goal; The "0" means
	% zero additional arguments will be appended to the argument

	:- public(whatever/1).
	:- meta_predicate(whatever(0)).
	:- mode(whatever(+callable), one).

	whatever(Goal) :-
		% succeed deterministically ignoring failure
		(Goal -> true; true).

	% what if we have a list of goals instead of a single goal?
	% in this case, we use "::" instead of "0" in the meta-predicate
	% template to inform the compiler that the meta-predicate
	% argument sub-terms are context-aware

	:- public(whatever_all/1).
	:- meta_predicate(whatever_all(::)).
	:- mode(whatever_all(+list(callable)), one).

	whatever_all([]).
	whatever_all([Goal| Goals]) :-
		whatever(Goal),
		whatever_all(Goals).

:- end_object.


:- object(simple_client).

	% predicates for testing calls to the
	% "simple" object meta-predicates

	:- public(test_whatever/0).

	test_whatever :-
		simple::whatever(true),
		simple::whatever(oops),
		simple::whatever(repeat).

	oops :- fail.

	:- public(test_whatever_all/0).

	test_whatever_all :-
		simple::whatever_all([foo,fail,bar,false,baz]).

	foo :- write('Hello ').
	bar :- write('world').
	baz :- write('!').

:- end_object.

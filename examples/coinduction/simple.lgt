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


:- object(simple).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/08/31,
		comment is 'Elementary coinduction predicate example.'
	]).

	:- public(p/0).
	:- coinductive(p/0).

	p :- p.

	:- public(p/1).
	:- coinductive(p/1).

	p(X) :- q(X).

	q(X) :- r(X).

	r(X) :- p(X).

	:- public(p/2).
	:- coinductive(p/2).

	p(_, Y) :-
		findall(T, s(T), Bag),
		member(Y, Bag).

	s(X) :- t(X).

	t(X) :- p(X, _).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.

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


% some simple predicates to use with library meta-predicates (e.g. fold_left/4
% and partition/4) compiled as plain Prolog code and thus defined in the "user"
% pseudo-object:

sum_squares(X, Y, Z) :-
	Z is X*X + Y*Y.


even_integer(I) :-
	I mod 2 =:= 0.


% a simple object defining some predicates to use with library meta-predicates:

:- object(predicates).

	:- info([
		version is 1.0,
		date is 2008/11/19,
		author is 'Paul Crocker',
		comment is 'Some predicates for testing the library meta-predicates.'
	]).

	:- public(tuple/3).

	tuple((X1, Y1), (X2, Y2), (X, Y)) :-
		X is X1 + X2,
		Y is Y1 + Y2.

	:- public(sum/3).

	sum(X, Y, Z) :-
		Z is X + Y.

	:- public(product/3).

	product(X, Y, Z) :-
		Z is X * Y.

:- end_object.

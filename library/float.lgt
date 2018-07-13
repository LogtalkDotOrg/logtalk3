%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(float,
	extends(number)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2018/07/13,
		comment is 'Floating point numbers data type predicates.'
	]).

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+float, +float), zero_or_one).
	:- mode('=~='(+list(float), +list(float)), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two floats (or lists of floats) for approximate equality using 100*epsilon for the absolute error and, if that fails, 99.999% accuracy for the relative error. Note that these precision values may not be adequate for all cases. No type-checking.',
		argnames is ['Float1', 'Float2']
	]).

	'=~='([], []) :-
		!.
	'=~='([Float1| Floats1], [Float2| Floats2]) :-
		!,
		'=~='(Float1, Float2),
		'=~='(Floats1, Floats2).
	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), abs(Float1 - Float2) < 100*Epsilon ->
			true
		;	% if that fails, test the relative error (99.999% accuracy):
			abs(Float1 - Float2) < 0.00001 * max(abs(Float1), abs(Float2))
		).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx)
	)).
		epsilon(Epsilon) :-
			{Epsilon is epsilon}.
	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).
		epsilon(Epsilon) :-
			{Epsilon is nexttoward(1.0, 2.0) - 1.0}.
	:- else.
		epsilon(0.000000000001).
	:- endif.

	valid(Float) :-
		float(Float).

	check(Term) :-
		(	float(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(float, Term)
		).

:- end_object.

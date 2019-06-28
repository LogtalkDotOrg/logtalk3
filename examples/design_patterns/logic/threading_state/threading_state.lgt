%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(states).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/01/31,
		comment is 'Example of threading state between predicate calls using Definite Clause Grammars (DCGs).'
	]).

	:- public(convert/2).
	:- mode(convert(+atom, -atom), one).
	:- info(convert/2, [
		comment is 'Converts a floating point number into an integer number by applying a sequence of operations.',
		argnames is ['Float', 'Integer']
	]).

	convert(Float, Integer) :-
		phrase(steps, Float, Integer).

	steps -->
		square,
		half,
		round.

	% we use a lambda expression to access the implicit
	% difference-list in each grammar rule

	square -->
		call([Number, Double]>>(Double is Number * Number)).

	half -->
		call([Number, Half]>>(Half is Number / 2.0)).

	round -->
		call([Float, Integer]>>(Integer is round(Float))).

	% alternative formulation where the individual steps
	% are defined as predicates instead of non-terminals
	%
	% the call//1 built-in meta non-terminal is used to
	% avoid hard-coding assumptions about how grammar
	% rules are compiled into clauses

%	steps -->
%		call(square),
%		call(half),
%		call(round).
%
%	square(Number, Double) :-
%		Double is Number*Number.
%
%	half(Number, Half) :-
%		Half is Number / 2.0.
%
%	round(Float, Integer) :-
%		Integer is round(Float).

:- end_object.

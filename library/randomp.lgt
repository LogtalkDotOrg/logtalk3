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



:- protocol(randomp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/10/31,
		comment is 'Random number generator protocol.'
	]).

	:- public(random/1).
	:- synchronized(random/1).
	:- mode(random(-float), one).
	:- info(random/1, [
		comment is 'Returns a new random float value in the interval [0.0, 1.0[.',
		argnames is ['Random']
	]).

	:- public(random/3).
	:- synchronized(random/3).
	:- mode(random(+integer, +integer, -integer), zero_or_one).
	:- mode(random(+float, +float, -float), zero_or_one).
	:- info(random/3, [
		comment is 'Returns a new random value in the interval [Lower, Upper[.',
		argnames is ['Lower', 'Upper', 'Random']
	]).

	:- public(randseq/4).
	:- synchronized(randseq/4).
	:- mode(randseq(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randseq(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randseq/4, [
		comment is 'Returns a list of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'List']
	]).

	:- public(randset/4).
	:- synchronized(randset/4).
	:- mode(randset(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randset(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randset/4, [
		comment is 'Returns an ordered set of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']
	]).

	:- public(reset_seed/0).
	:- synchronized(reset_seed/0).
	:- mode(reset_seed, one).
	:- info(reset_seed/0, [
		comment is 'Resets the random seed to its default value.'
	]).

	:- public(set_seed/1).
	:- synchronized(set_seed/1).
	:- mode(set_seed(+integer), zero_or_one).
	:- info(set_seed/1, [
		comment is 'Sets the random seed to the given value.',
		argnames is ['Seed']
	]).

:- end_protocol.

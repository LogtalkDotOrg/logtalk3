%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.1,
		author is 'Paulo Moura',
		date is 2017/02/25,
		comment is 'Random number generator protocol.'
	]).

	:- public(random/1).
	:- mode(random(-float), one).
	:- info(random/1, [
		comment is 'Returns a new random float value in the interval [0.0, 1.0[.',
		argnames is ['Random']
	]).

	:- public(between/3).
	:- mode(between(+integer, +integer, -integer), zero_or_one).
	:- info(between/3, [
		comment is 'Returns a new random integer in the interval [Lower, Upper]. Fails if Lower or Upper are not integers or if Lower > Upper.',
		argnames is ['Lower', 'Upper', 'Random']
	]).

	:- public(member/2).
	:- mode(member(-term, +list(term)), zero_or_one).
	:- info(member/2, [
		comment is 'Returns a random member of a list. Fails if the list is empty.',
		argnames is ['Random', 'List']
	]).

	:- public(select/3).
	:- mode(select(-term, +list(term), -list(term)), zero_or_one).
	:- info(select/3, [
		comment is 'Returns a random member of a list and the rest of the list. Fails if the list is empty.',
		argnames is ['Random', 'List', 'Rest']
	]).

	:- public(permutation/2).
	:- mode(permutation(+list, -list), one).
	:- info(permutation/2, [
		comment is 'Returns a random permutation of a list.',
		argnames is ['List', 'Permutation']
	]).

	:- public(sequence/4).
	:- mode(sequence(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- info(sequence/4, [
		comment is 'Returns a list of the given length of random integers in random order in the interval [Lower, Upper]. Fails if Length, Lower, or Upper are not integers or if Lower > Upper.',
		argnames is ['Length', 'Lower', 'Upper', 'List']
	]).

	:- public(set/4).
	:- mode(set(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- info(set/4, [
		comment is 'Returns an ordered set of the given size of random integers in the interval [Lower, Upper]. Fails if Length, Lower, or Upper are not integers, if Lower > Upper, or if Length > Upper - Lower + 1.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']
	]).

	:- public(random/3).
	:- mode(random(+integer, +integer, -integer), zero_or_one).
	:- mode(random(+float, +float, -float), zero_or_one).
	:- info(random/3, [
		comment is 'Returns a new random value in the interval [Lower, Upper[. Fails if Lower > Upper. Deprecated. Use between/3 for integers.',
		argnames is ['Lower', 'Upper', 'Random']
	]).

	:- public(randseq/4).
	:- mode(randseq(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randseq(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randseq/4, [
		comment is 'Returns a list of the given length of random values in random order in the interval [Lower, Upper[. Fails if Lower > Upper or if the arguments are neither integers or floats. Deprecated. Use sequence/4 for integers.',
		argnames is ['Length', 'Lower', 'Upper', 'List']
	]).

	:- public(randset/4).
	:- mode(randset(+integer, +integer, +integer, -list(integer)), zero_or_one).
	:- mode(randset(+integer, +float, +float, -list(float)), zero_or_one).
	:- info(randset/4, [
		comment is 'Returns an ordered set of the given size of random values in the interval [Lower, Upper[. Fails if the arguments are neither integers or floats, Lower > Upper, or Length > Upper - Lower when the arguments are integers. Deprecated. Use set/4 for integers.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']
	]).

	:- public(reset_seed/0).
	:- mode(reset_seed, one).
	:- info(reset_seed/0, [
		comment is 'Resets the random generator seed to its default value. Use get_seed/1 and set_seed/1 instead if you need reproducibility.'
	]).

	:- public(get_seed/1).
	:- mode(get_seed(-ground), one).
	:- info(get_seed/1, [
		comment is 'Gets the current random generator seed. Seed should be regarded as an opaque ground term.',
		argnames is ['Seed']
	]).

	:- public(set_seed/1).
	:- mode(set_seed(+ground), one).
	:- info(set_seed/1, [
		comment is 'Sets the random generator seed to a given value returned by calling the get_seed/1 predicate.',
		argnames is ['Seed']
	]).

	:- public(randomize/1).
	:- mode(randomize(+positive_integer), one).
	:- info(randomize/1, [
		comment is 'Randomizes the random generator using a positive integer to compute a new seed.',
		argnames is ['Seed']
	]).

:- end_protocol.

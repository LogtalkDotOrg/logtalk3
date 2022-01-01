%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- protocol(pseudo_random_protocol,
	extends(random_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-02-21,
		comment is 'Pseudo-random number generator protocol for seed handling predicates. These predicates are declared as synchronized when the library is compiled using a backend supporting threads.',
		see_also is [random, backend_random, fast_random]
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
		comment is 'Sets the random generator seed to a given value returned by calling the ``get_seed/1`` predicate.',
		argnames is ['Seed']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([get_seed/1, set_seed/1]).
	:- endif.

:- end_protocol.

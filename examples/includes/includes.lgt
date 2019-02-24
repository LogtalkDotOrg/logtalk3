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


% note that the following directive is compiled as a *Logtalk* include/1
% directive, *not* as a Prolog include/1 directive to avoid portability
% issues between backend Prolog compilers
:- include('vowels.pl').


:- object(countries).

	:- public([
		capitals/1,
		same_population/1
	]).

	% load the countries database file when the object is compiled and loaded
	:- include('countries.pl').

	capitals(Capitals) :-
		setof(Capital, Country^Population^country(Country, Capital, Population), Capitals).

	same_population(Countries) :-
		setof(Country, Capital^country(Country, Capital, _Population), Countries).

:- end_object.

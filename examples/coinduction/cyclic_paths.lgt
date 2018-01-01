%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- object(cp1).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, b).	arc(b, c).
	arc(c, d).	arc(c, a).
	arc(d, d).

:- end_object.



:- object(cp2).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, c).
	arc(c, a).	arc(c, d).
	arc(d, a).

:- end_object.



:- object(cp3).

	:- info([
		version is 1.1,
		author is 'Paulo Moura. Derived from a Gopal Gupta et al example.',
		date is 2013/03/06,
		comment is 'Coinduction example of finding the cyclic paths in a graph.'
	]).

	:- public(path/3).

	:- private(path/4).
	:- coinductive(path(+, +, -, -)).

	path(From, Path, MaxLength) :-
		path(From, Path, 0, MaxLength).

	path(From, [From| Path], Length, MaxLength) :-
		Length < MaxLength,
		Length1 is Length + 1,
		arc(From, Next),
		path(Next, Path, Length1, MaxLength).

	arc(a, b).
	arc(b, c).
	arc(c, a).	arc(c, d).
	arc(d, a).

:- end_object.

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


:- object(ack).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/3/31,
		comment is 'Ackermann function (general recursive function).'
	]).

	:- public(ack/3).
	:- mode(ack(+integer, +integer, -integer), one).
	:- info(ack/3, [
		comment is 'Ackermann function.',
		argnames is ['M', 'N', 'V']
	]).

	ack(0, N, V) :-
		!,
		V is N + 1.
	ack(M, 0, V) :-
		!,
		M2 is M - 1,
		ack(M2, 1, V).
	ack(M, N, V) :-
		M2 is M - 1,
		N2 is N - 1,
		ack(M, N2, V2),
		ack(M2, V2, V).

:- end_object.

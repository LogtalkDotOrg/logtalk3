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


:- object(bom).

	:- info([
		version is 1.0,
		date is 2004/5/11,
		author is 'Paulo Moura',
		comment is 'Adaptation of the bill of materials DCG example from the Amzi! Prolog manual.'
	]).

	:- public(parts/2).
	:- mode(parts(+atom, -list), one).
	:- info(parts/2, [
		comment is 'Returns the list of parts for building an object.',
		argnames is ['Object', 'Parts']
	]).

	parts(Object, Parts) :-
		phrase(Object, Parts).

	bike --> frame, drivechain, wheel, wheel.

	wheel --> spokes, rim, hub.

	drivechain --> crank, pedal, pedal, chain.

	spokes --> [spokes].
	crank --> [crank].
	pedal --> [pedal].
	chain --> [chain].
	rim --> [rim].
	hub --> [hub].
	frame --> [frame].

:- end_object.

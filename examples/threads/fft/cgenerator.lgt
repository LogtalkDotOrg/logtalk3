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


:- object(cgenerator).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/2/14,
		comment is 'Simple object defining a predicate for generating lists of random complex numbers.'
	]).

	:- public(list/2).

	list(N, Cs) :-
		N2 is N*2,
		random::randseq(N2, 0.0, 1.0, Fs),
		convert(Fs, Cs).

	convert([], []).
	convert([F1, F2 | Fs] , [c(F1, F2)| Cs]) :-
		convert(Fs, Cs).

:- end_object.

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


% example of using Logtalk's meta_non_terminal/1 directive plus call//N
% built-in non-terminal
%
% derived from a related question by Richard O'Keefe on the SWI-Prolog
% mailing list on May 6, 2012

:- object(dcg).

	:- public(list//2).
	:- meta_non_terminal(list(1, *)).

	:- meta_non_terminal(phrase(1, *)).

	list(_, []) --> [].
	list(X, [T|Ts]) --> phrase(X, T), list(X, Ts).

	% phrase//1-N is more general than call//1-N but only the
	% latter is currently provided as a built-in non-terminal
	phrase(X, T) --> call(X, T).

:- end_object.



:- object(client).

	:- public(print/0).

	print :-
		phrase(dcg::list(print, [1,2,3]), [one, two, three]),
		phrase(dcg::list(print, [a,b,c]), [one, two, three]).

	print(N, [Element| Result], Result) :-
		write(N-Element), nl.

	:- public(successors/2).

	successors(Elements, Successors) :-
		phrase(dcg::list(next, Successors), Elements).

	next(Next, [Element| Result], Result) :-
		Next is Element + 1.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(object).

	:- info([
		version is 1:0:0,
		author is pm,
		date is 2008-4-9,
		comment is 'Simple example of using compilation hooks and term expansion for conditional compilation of debug statements.'
	]).

	:- public(append/3).

	append([], List, List) :-
		debug((write('Base case: '), writeq(append([], List, List)), nl)).
	append([Head| Tail], List, [Head| Tail2]) :-
		debug((write('Recursive case: '), writeq(append(Tail, List, Tail2)), nl)),
		append(Tail, List, Tail2).

	:- public(sum/2).

	sum(List, Sum) :-
		debug(list::check(List)),
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		debug(number::check(X)),
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(lambda_warnings).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2019-08-17,
		comment is 'Example for illustrating lambda compilation warnings.'
	]).

	% lambda expression with unclassified variables
	foo(C1, C2, C3) :-
		g(Z),
		call([X,Y]>>f(X,Y,Z), C1, C2, C3).

	% lambda expression with mixed-up variables
	bar(C1, C2, C3) :-
		call({Z}/[X,Y,Z]>>f(X,Y,Z), C1, C2, C3).

	% lambda expression with parameter variables occurring elsewhere
	baz(X, C1, C2) :-
		g(X),
		call([X,Y]>>f(X,Y), C1, C2).

	f(1, 2, 3).

	g(4).

	f(1, 2).

:- end_object.

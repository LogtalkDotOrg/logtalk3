%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2015/09/02,
		comment is 'Unit tests for the "wrappers" example.'
	]).

	test(wrappers_1) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Next == zip([3, 2, 1], 4, [5]).

	test(wrappers_2) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), next(Zip, Next), previous(Next, Zip)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Next == zip([3, 2, 1], 4, [5]).

	test(wrappers_3) :-
		zipper<<(zipper(3, [1,2,3,4,5], Zip, X), previous(Zip, Previous)),
		Zip == zip([2, 1], 3, [4, 5]),
		X == 3,
		Previous == zip([1], 2, [3, 4, 5]).

	test(wrappers_4) :-
		zipper<<(zipper(3, [1,2,3,4,5], Three, X), next(Three, Four), next(Four, Five), previous(Five, Four), previous(Four, Three), previous(Three, Two), previous(Two, One)),
		Three == zip([2, 1], 3, [4, 5]),
		X == 3,
		Four == zip([3, 2, 1], 4, [5]),
		Five == zip([4, 3, 2, 1], 5, []),
		Two == zip([1], 2, [3, 4, 5]),
		One == zip([], 1, [2, 3, 4, 5]).

:- end_object.

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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-05-29,
		comment is 'Unit tests for the "process_modeling" example.'
	]).

	cover(a(_)).
	cover(b(_)).
	cover(c(_)).
	cover(process_model).

	test(process_modeling_01, true(Results == [2-[a(2),b(2)], 3-[a(2),b(3)], 3-[a(3),b(3)]])) :-
		findall(
			B-Dependencies,
			process_model::solve([b(B)], Dependencies),
			Results
		).

	test(process_modeling_02, true(Results == [3-[b(2),a(2),c(3)], 4-[b(3),a(3),c(4)]])) :-
		findall(
			C-Dependencies,
			process_model::solve([c(C)], Dependencies),
			Results
		).

:- end_object.

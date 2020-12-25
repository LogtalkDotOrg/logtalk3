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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-12-24,
		comment is 'Unit tests for the "now_you_see_me" example.'
	]).

	cover(four_horseman).
	cover(magic).
	cover(stage).

	test(now_you_see_me_01, true(AllHorseman == [danny, merritt, henley, jack])) :-
		magic::show,
		stage::all(AllHorseman).

	test(now_you_see_me_02, true(AllHorseman == [])) :-
		magic::hide,
		stage::all(AllHorseman).

	test(now_you_see_me_03, true(AllHorseman == [danny, merritt, henley, jack])) :-
		magic::show,
		stage::all(AllHorseman).

:- end_object.

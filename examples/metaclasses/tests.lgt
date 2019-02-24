%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		date is 2019/01/19,
		comment is 'Unit tests for the "metaclasses" example.'
	]).

	cover(metaclass).
	cover(root).
	cover(subclass1).
	cover(subclass2).

	test(metaclasses_01) :-
		root::new(Instance),
		instantiates_class(Instance, Class),
		Class == root.

	test(metaclasses_02) :-
		subclass1::new(Instance),
		instantiates_class(Instance, Class),
		Class == subclass1.

	test(metaclasses_03) :-
		subclass2::new(Instance),
		instantiates_class(Instance, Class),
		Class == subclass2.

:- end_object.

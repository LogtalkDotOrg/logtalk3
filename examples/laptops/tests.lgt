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
		date is 2017/02/27,
		comment is 'Unit tests for the "laptops" example.'
	]).

	test(laptops_1) :-
		custom::new(fast, fifteen, querty, Laptop),
		imports_category(Laptop, Category),
		Category == laptop.

	test(laptops_2) :-
		custom::new(faster, thirteen, dvorak, Laptop),
		Laptop::mainboard(Mainboard), Mainboard == faster,
		Laptop::display(Display), Display == thirteen,
		Laptop::keyboard(Keyboard), Keyboard == dvorak.

:- end_object.

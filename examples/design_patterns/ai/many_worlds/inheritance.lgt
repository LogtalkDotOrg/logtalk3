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


% in this simple implementation of the "many worlds" pattern, the different
% worlds inherite the reasoning code


% we use a category instead on an object to prevent sending reasoning messages
% outside the context of a specific world; if that is not concern, an object
% could be used; yet another alternative would be to use classes with the
% specific worlds as instances

:- category(reasoner).

	:- public([
		value/1,
		lowest/1, highest/1, average/1
	]).

	:- uses(numberlist, [
		min/2, max/2, average/2
	]).

	lowest(Lowest) :-
		findall(Value, ::value(Value), Values),
		min(Values, Lowest).

	highest(Highest) :-
		findall(Value, ::value(Value), Values),
		max(Values, Highest).

	average(Average) :-
		findall(Value, ::value(Value), Values),
		average(Values, Average).

:- end_category.


:- object(world1,
	imports(reasoner)).

	value(23.7).
	value(17.8).
	value(25.1).

:- end_object.


:- object(world2,
	imports(reasoner)).

	value(13.7).
	value(9.8).
	value(11.2).

:- end_object.

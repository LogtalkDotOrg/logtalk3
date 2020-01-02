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


% in this simple implementation of the "many worlds" pattern, the different
% worlds are passed to the reasoning object as parameters


% to avoid repreating predicate declarations, we define a protocol for the
% dataset (or world) predicates

:- protocol(dataset_protocol).

	:- public(value/1).

:- end_protocol.


% the specific datasets (worlds) implement the protocol

:- object(dataset1,
	implements(dataset_protocol)).

	value(23.7).
	value(17.8).
	value(25.1).

:- end_object.


:- object(dataset2,
	implements(dataset_protocol)).

	value(13.7).
	value(9.8).
	value(11.2).

:- end_object.


% the reasoner object takes a dataset (world) as parameter

:- object(reasoner(_Dataset_)).

	:- public([
		lowest/1, highest/1, average/1
	]).

	% the uses/2 directive supports implicit messages
	% to an object that are only know at runtime
	:- uses(_Dataset_, [
		value/1
	]).

	:- uses(numberlist, [
		min/2, max/2, average/2
	]).

	% due to the uses/2 directive for the paramter, the value/1
	% goals below are compiled as _Dataset_::value/1 messages

	lowest(Lowest) :-
		findall(Value, value(Value), Values),
		min(Values, Lowest).

	highest(Highest) :-
		findall(Value, value(Value), Values),
		max(Values, Highest).

	average(Average) :-
		findall(Value, value(Value), Values),
		average(Values, Average).

:- end_object.

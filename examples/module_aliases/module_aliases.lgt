%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(data_source).

	% editing the module in the following directive would avoid having
	% to change all explicitly-qualified predicate calls to it as they
	% all would be written using the module alias
	:- use_module([
		data1 as data
%		data2 as data
	]).

	:- public(all/1).
	all(Data) :-
		findall(X, data:a(X), Data).

:- end_object.



:- object(pardata(_DataModule_)).

	% allow module parametrization for all explicitly-qualified
	% predicate calls by simply defining a module alias
	:- use_module([
		_DataModule_ as data
	]).

	:- public(all/1).
	all(Data) :-
		findall(X, data:a(X), Data).

:- end_object.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2022-11-23,
		comment is 'Unit tests for the "ids" library.'
	]).

	cover(ids).
	cover(ids(_, _)).

	quick_check(id_default_valid, ids(-atom)).
	quick_check(id_valid(atom),  ids({atom},  +between(integer,1,30), -atom)).
	quick_check(id_valid(chars), ids({chars}, +between(integer,1,30), -chars)).
	quick_check(id_valid(codes), ids({codes}, +between(integer,1,30), -codes)).

	ids(Identifier) :-
		ids::generate(Identifier).

	ids(Representation, Bytes, Identifier) :-
		ids(Representation, Bytes)::generate(Identifier).

:- end_object.

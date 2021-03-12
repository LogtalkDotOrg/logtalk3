%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-03-12,
		comment is 'Unit tests for the "uuid" library.'
	]).

	cover(uuid).
	cover(uuid(_)).

	quick_check(uuid_v1_valid, uuid_v1_valid({[0xf2,0xd1,0x90,0x94,0xdc,0x4b]}, -chars)).
	quick_check(uuid_v4_valid, uuid_v4_valid(-chars)).

	% auxiliary predicates

	uuid_v1_valid(MAC, UUID) :-
		uuid(chars)::uuid_v1(MAC, UUID),
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == '-',
		Version == '1',
		once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

	uuid_v4_valid(UUID) :-
		uuid(chars)::uuid_v4(UUID),
		UUID = [
			_, _, _, _, _, _, _, _, Dash,
			_, _, _, _, Dash,
			Version, _, _, _, Dash,
			Hex, _, _, _, Dash,
			_, _, _, _, _, _, _, _, _, _, _, _
		],
		Dash == '-',
		Version == '4',
		once((Hex == '8'; Hex == '9'; Hex == 'a'; Hex == 'b')).

:- end_object.

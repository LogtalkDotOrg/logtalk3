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


:- object(object).

	:- info([
		version is 1:21:0,
		author is pm,
		date is 2012-08-02,
		comment is 'Example object for illustrating the use of compiler hooks.',
		license is gpl3]).

	:- public(out/0).

	out :-
		write('A'), nl,
		write(x(A, A)), nl,
		write(3), nl.

	:- local_data(item/1).

	item(zeta).
	item(omega).
	item(alpha).

	:- public(items/1).

	items(Items) :-
		findall(Item, item(Item), Items).

:- end_object.

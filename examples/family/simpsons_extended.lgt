%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(simpsons_extended,
	extends(simpsons)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2015-09-23,
		comment is 'Extended Simpsons family.'
	]).

	male(Male) :-
		^^male(Male).
	male(abe).
	male(herb).

	female(Female) :-
		^^female(Female).
	female(gaby).
	female(mona).

	parent(Parent, Child) :-
		^^parent(Parent, Child).
	parent(abe, homer).
	parent(abe, herb).
	parent(gaby, herb).
	parent(mona, homer).

:- end_object.

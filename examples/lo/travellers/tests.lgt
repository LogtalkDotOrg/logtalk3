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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:1,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the "lo/travellers" example.'
	]).

	test(lo_travellers_1) :-
		incremental::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == oxford-london-portsmouth-brighton-exeter-aberystwyth.

	test(lo_travellers_2) :-
		presort::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == brighton-london-oxford-portsmouth-exeter-aberystwyth.

	test(lo_travellers_3) :-
		circular::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == london-brighton-portsmouth-exeter-aberystwyth-oxford-london.

	test(lo_travellers_4) :-
		permute::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		subsumes_term((aberystwyth-exeter-portsmouth-brighton-london-oxford,_), Route).

:- end_object.

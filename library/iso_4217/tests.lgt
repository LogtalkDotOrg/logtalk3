%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2026-05-05,
		comment is 'Scoped unit tests for the "iso_4217" library scaffolding.'
	]).

	cover(iso_4217).

	test(currency_05_01, deterministic(MinorUnit == 2)) :-
		iso_4217::currency('AFN', 971, MinorUnit, 'Afghani', 'AFGHANISTAN').

	test(currency_05_02, deterministic(MinorUnit == na)) :-
		iso_4217::currency('XDR', 960, MinorUnit, 'SDR (Special Drawing Right)', 'INTERNATIONAL MONETARY FUND (IMF)').

	test(fund_currency_05_01, deterministic(MinorUnit == 2)) :-
		iso_4217::fund_currency('BOV', 984, MinorUnit, 'Mvdol', 'BOLIVIA (PLURINATIONAL STATE OF)').

	test(currency_05_03, fail) :-
		iso_4217::currency('BOV', _, _, _, _).

	test(currency_05_04, fail) :-
		iso_4217::currency(zzz, _, _, _, _).

	test(fund_currency_05_02, fail) :-
		iso_4217::fund_currency(zzz, _, _, _, _).

:- end_object.

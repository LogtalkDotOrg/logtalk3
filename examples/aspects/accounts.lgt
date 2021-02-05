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


:- category(account).

	:- public(withdraw/1).
	withdraw(Amount) :-
		::balance_(Balance),
		Balance >= Amount,
		::retract(balance_(Balance)),
		NewBalance is Balance - Amount,
		::assertz(balance_(NewBalance)).

	:- public(deposit/1).
	deposit(Amount) :-
		::retract(balance_(Balance)),
		NewBalance is Balance + Amount,
		::assertz(balance_(NewBalance)).

	:- public(balance/1).
	balance(Balance) :-
		::balance_(Balance).

	:- protected(balance_/1).
	:- dynamic(balance_/1).

:- end_category.


:- object(john,
	imports(account)).

	:- dynamic(balance_/1).
	balance_(1000).

:- end_object.


:- object(jane,
	imports(account)).

	:- dynamic(balance_/1).
	balance_(1000).

:- end_object.

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


:- object(test_subject,
	imports(subject)).

:- end_object.


:- object(test_observer,
	imports(observer)).

	:- public(notifications/1).
	:- mode(notifications(-list), one).

	:- public(reset/0).
	:- mode(reset, one).

	:- private(notification_/1).
	:- dynamic(notification_/1).

	update(Change) :-
		^^update(Change),
		assertz(notification_(Change)).

	notifications(Notifications) :-
		findall(Notification, notification_(Notification), Notifications).

	reset :-
		retractall(notification_(_)).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-09,
		comment is 'Unit tests for the "dependents" library.'
	]).

	cover(subject).
	cover(observer).

	cleanup :-
		test_subject::removeDependent(_),
		fail.
	cleanup :-
		test_observer::reset.

	test(dependents_initial_01, deterministic(Dependents == []), [setup(cleanup)]) :-
		test_subject::dependents(Dependents).

	test(dependents_add_01, deterministic(Dependents == [test_observer]), [setup(cleanup)]) :-
		test_subject::addDependent(test_observer),
		test_subject::dependents(Dependents).

	test(dependents_add_duplicate_01, deterministic(Dependents == [test_observer]), [setup(cleanup)]) :-
		test_subject::addDependent(test_observer),
		test_subject::addDependent(test_observer),
		test_subject::dependents(Dependents).

	test(dependents_changed_0_01, deterministic(Notifications == [test_subject]), [setup(cleanup)]) :-
		test_subject::addDependent(test_observer),
		test_subject::changed,
		test_observer::notifications(Notifications).

	test(dependents_changed_1_01, deterministic(Notifications == [value(42)]), [setup(cleanup)]) :-
		test_subject::addDependent(test_observer),
		test_subject::changed(value(42)),
		test_observer::notifications(Notifications).

	test(dependents_remove_01, deterministic(Dependents-Notifications == []-[]), [setup(cleanup)]) :-
		test_subject::addDependent(test_observer),
		test_subject::removeDependent(test_observer),
		test_subject::dependents(Dependents),
		test_subject::changed(ignored),
		test_observer::notifications(Notifications).

:- end_object.

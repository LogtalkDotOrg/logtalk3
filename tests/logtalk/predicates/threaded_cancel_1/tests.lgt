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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-02-12,
		comment is 'Unit tests for the threaded_cancel/1 built-in predicate.'
	]).

	throws(threaded_cancel_1_01, error(instantiation_error, logtalk(threaded_cancel(_), _))) :-
		{threaded_cancel(_)}.

	succeeds(threaded_cancel_1_02) :-
		{threaded_call(true, Tag),
		 threaded_cancel(Tag)}.

	succeeds(threaded_cancel_1_03) :-
		{threaded_once(true, Tag),
		 threaded_cancel(Tag)}.

:- end_object.

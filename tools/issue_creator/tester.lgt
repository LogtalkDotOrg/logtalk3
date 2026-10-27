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


:- object(issue_creator_test_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-26,
		comment is 'Omits the issue reporting callback when compiling the tool for offline tests.'
	]).

	term_expansion((logtalk::message_hook(_, _, lgtunit, _) :- _), []).

:- end_object.


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(lgtunit(loader)),
	logtalk_load(git(loader)),
	logtalk_load(os(loader)),
	logtalk_load(term_io(loader)),
	logtalk_load(base64(loader)),
	logtalk_load(character_sets(loader)),
	(	current_object(issue_creator) ->
		% assume tool already loaded, likely running a CI/CD pipeline
		true
	;	logtalk_load(issue_creator, [hook(issue_creator_test_expansion), source_data(on), debug(on)])
	),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).

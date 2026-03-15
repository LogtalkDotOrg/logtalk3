%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2018-2026 Paulo Moura <pmoura@logtalk.org>
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


:- if(
	(	os::operating_system_type(windows) ->
		os::shell('where cloc > NUL 2>&1'),
		os::shell('where sed > NUL 2>&1')
	;	os::shell('command -v cloc >/dev/null 2>&1')
	)
).

:- object(lines_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Unit tests for the lines_metric object.'
	]).

	:- uses(lines_metric, [
		all/0,
		rlibrary/1,
		library/1,
		rdirectory/1,
		directory/1,
		file/1,
		entity/1,
		entity_score/2
	]).

	cover(code_metric).
	cover(lines_metric).

	test(lines_metric_entity, deterministic) :-
		entity(lcom_obj_1).

	test(lines_metric_entity_score_shape, true((Code >= 0, Comments >= 0, Blanks >= 0, Total > 0))) :-
		entity_score(lcom_obj_1, lines(Code, Comments, Blanks)),
		Total is Code + Comments + Blanks.

	test(lines_metric_file, deterministic) :-
		object_property(lcom_obj_1, file(File)),
		file(File).

	test(lines_metric_library, deterministic) :-
		library(core).

	test(lines_metric_rlibrary, deterministic) :-
		rlibrary(core).

	test(lines_metric_directory, deterministic) :-
		logtalk::expand_library_path(core, Directory),
		directory(Directory).

	test(lines_metric_rdirectory, deterministic) :-
		logtalk::expand_library_path(core, Directory),
		rdirectory(Directory).

	test(lines_metric_all, deterministic) :-
		all.

:- end_object.

:- else.

:- object(lines_metric_tests,
	extends(lgtunit)).
:- end_object.

:- endif.

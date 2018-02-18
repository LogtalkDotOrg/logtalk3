%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(code_metrics_tests,
	extends(lgtunit)).

	:- info([
		version is 0.6,
		author is 'Ebrahim Azarisooreh',
		date is 2018/02/18,
		comment is 'Unit tests for code metrics framework.'
	]).

	cover(code_metric).
	cover(code_metrics_utilities).
	cover(code_metrics).

	:- uses(code_metrics, [
		all/0,
		rlibrary/1,
		library/1,
		rdirectory/1,
		directory/1,
		file/1,
		entity/1
	]).

	test(code_metrics_entity, deterministic) :-
		entity(obj_c).

	test(code_metrics_file, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(code_metrics_library, deterministic) :-
		library(lgtunit).

	test(code_metrics_rlibrary, deterministic) :-
		rlibrary(lgtunit).

	test(code_metrics_directory, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(code_metrics_rdirectory, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	test(code_metrics_all, deterministic) :-
		all.

	% suppress all messages from the "code_metrics"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

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


:- object(size_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Ebrahim Azarisooreh',
		date is 2018/01/23,
		comment is 'Unit tests for the source code size metric.'
	]).

	cover(code_metric).
	cover(size_metric_utilities).
	cover(size_metric).

	:- uses(size_metric, [
		all/0,
		rlibrary/1,
		library/1,
		rdirectory/1,
		directory/1,
		file/1,
		entity/1
	]).

	:- uses(lgtunit, [
		deterministic/1
	]).

	test(size_metric_entity) :-
		deterministic(entity(logtalk)).

	test(size_metric_file) :-
		object_property(logtalk, file(File)),
		deterministic(file(File)).

	test(size_metric_library) :-
		deterministic(library(core)).

	test(size_metric_rlibrary) :-
		deterministic(rlibrary(core)).

	test(size_metric_directory) :-
		logtalk::expand_library_path(core, Directory),
		deterministic(directory(Directory)).

	test(size_metric_rdirectory) :-
		logtalk::expand_library_path(core, Directory),
		deterministic(rdirectory(Directory)).

	test(size_metric_all) :-
		deterministic(all).

	% suppress all messages from the "code_metrics"
	% tool to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, code_metrics, _Tokens).

:- end_object.

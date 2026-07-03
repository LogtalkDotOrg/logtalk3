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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'Performance benchmarks for the JSON Graph Format v2 library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(car_graphs_parse_generate_benchmark, true, [note(metrics(file-'car_graphs.json', parse_seconds-ParseTime, generate_seconds-GenerateTime, term_count-Count))]) :-
		benchmark_fixture('test_files/car_graphs.json', ParseTime, GenerateTime, Count).

	test(test_network_parse_generate_benchmark, true, [note(metrics(file-'test.network.json', parse_seconds-ParseTime, generate_seconds-GenerateTime, term_count-Count))]) :-
		benchmark_fixture('test_files/test.network.json', ParseTime, GenerateTime, Count).

	test(les_miserables_parse_generate_benchmark, true, [note(metrics(file-'les_miserables.json', parse_seconds-ParseTime, generate_seconds-GenerateTime, term_count-Count))]) :-
		benchmark_fixture('test_files/les_miserables.json', ParseTime, GenerateTime, Count).

	test(hyper_directed_parse_generate_benchmark, true, [note(metrics(file-'hyper-directed.json', parse_seconds-ParseTime, generate_seconds-GenerateTime, term_count-Count))]) :-
		benchmark_fixture('test_files/hyper-directed.json', ParseTime, GenerateTime, Count).

	test(hyper_undirected_parse_generate_benchmark, true, [note(metrics(file-'hyper-undirected.json', parse_seconds-ParseTime, generate_seconds-GenerateTime, term_count-Count))]) :-
		benchmark_fixture('test_files/hyper-undirected.json', ParseTime, GenerateTime, Count).

	benchmark_fixture(File, ParseTime, GenerateTime, Count) :-
		^^file_path(File, Path),
		benchmark(json_graph::parse(file(Path), _), ParseTime),
		json_graph::parse(file(Path), Terms),
		length(Terms, Count),
		Count > 0,
		benchmark(json_graph::generate(atom(_), Terms), GenerateTime).

:- end_object.

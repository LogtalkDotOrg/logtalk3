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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2025-12-07,
		comment is 'Performance tests for the "toon" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2, benchmark/3
	]).

	% Performance test: parse github_repos.toon (100 records, tabular format)
	test(parse_github_repos_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/github_repos.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_github_repos_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/github_repos.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse employees.toon (50 records, tabular format)
	test(parse_employees_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/employees.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_employees_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/employees.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse orders.toon (20 nested objects, expanded format)
	test(parse_orders_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/orders.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_orders_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/orders.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse analytics.toon (60 records, tabular format)
	test(parse_analytics_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/analytics.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_analytics_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/analytics.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse config.toon (deeply nested object)
	test(parse_config_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/config.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_config_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/config.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse event_logs.toon (75 records, tabular format)
	test(parse_event_logs_100_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/event_logs.toon', Path),
		benchmark(toon::parse(file(Path), _), 100, Time).

	test(parse_event_logs_1000_times, true, [note(seconds-Time)]) :-
		^^file_path('test_files/large/event_logs.toon', Path),
		benchmark(toon::parse(file(Path), _), 1000, Time).

	% Performance test: parse all large files once
	test(parse_all_large_files, true, [note(seconds-Time)]) :-
		benchmark(parse_all_large_files_, Time).

	parse_all_large_files_ :-
		^^file_path('test_files/large/github_repos.toon', GithubRepos),
		toon::parse(file(GithubRepos), _),
		^^file_path('test_files/large/employees.toon', Employees),
		toon::parse(file(Employees), _),
		^^file_path('test_files/large/orders.toon', Orders),
		toon::parse(file(Orders), _),
		^^file_path('test_files/large/analytics.toon', Analytics),
		toon::parse(file(Analytics), _),
		^^file_path('test_files/large/config.toon', Config),
		toon::parse(file(Config), _),
		^^file_path('test_files/large/event_logs.toon', EventLogs),
		toon::parse(file(EventLogs), _).

:- end_object.

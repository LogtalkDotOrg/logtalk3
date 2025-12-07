%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2025-12-07,
		comment is 'Performance tests for the "ccsds" library.'
	]).

	:- private(bytes_/1).
	:- dynamic(bytes_/1).

	setup :-
		type::arbitrary(ccsds_packet, Bytes),
		assertz(bytes_(Bytes)).

	cleanup :-
		retractall(bytes_(_)).

	% Performance test: parse 1 packet 1000 times
	test(parse_1_packet_1000_times, true, [note(seconds-Time)]) :-
		bytes_(Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), 1000, Time).

	% Performance test: parse 1 packet 10000 times
	test(parse_1_packet_10000_times, true, [note(seconds-Time)]) :-
		bytes_(Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), 10000, Time).

	% Performance test: parse 1 packet 100000 times
	test(parse_1_packet_100000_times, true, [note(seconds-Time)]) :-
		bytes_(Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), 100000, Time).

	% Performance test: parse 1000 packets
	test(parse_1000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(ccsds_packets(1000), Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), Time).

	% Performance test: parse 10000 packets
	test(parse_10000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(ccsds_packets(10000), Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), Time).

	% Performance test: parse 100000 packets
	test(parse_100000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(ccsds_packets(100000), Bytes),
		lgtunit::benchmark(ccsds::parse(bytes(Bytes), _), Time).

:- end_object.


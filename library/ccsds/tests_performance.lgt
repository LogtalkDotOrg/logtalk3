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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2025-12-06,
		comment is 'Performance tests for the "ccsds" library.'
	]).

	% Performance test: parse 1000 packets
	test(parse_1000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(list(ccsds_packet, 1000), Packets),
		list::flatten(Packets, Bytes),
		lgtunit::benchmark(ccsds::parse_all(Bytes, _), Time).

	% Performance test: parse 10000 packets
	test(parse_10000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(list(ccsds_packet, 10000), Packets),
		list::flatten(Packets, Bytes),
		lgtunit::benchmark(ccsds::parse_all(Bytes, _), Time).

	% Performance test: parse 100000 packets
	test(parse_100000_packets, true, [note(seconds-Time)]) :-
		type::arbitrary(list(ccsds_packet, 100000), Packets),
		list::flatten(Packets, Bytes),
		lgtunit::benchmark(ccsds::parse_all(Bytes, _), Time).

:- end_object.


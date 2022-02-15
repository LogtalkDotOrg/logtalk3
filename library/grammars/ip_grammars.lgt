%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(ip_grammars(_Format_)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2022-02-15,
		comment is 'IP address grammars.',
		parnames is ['Format']
	]).

	:- public(ipv4//1).
	:- mode(ipv4(?list(integer)), zero_or_one).
	:- info(ipv4//1, [
		comment is 'Parses an IPv4 network address in the format XXX.XXX.XXX.XXX where each XXX is an octet (i.e. an integer between 0 and 255).'
	]).

	:- public(ipv6//1).
	:- mode(ipv6(?list(integer)), one).
	:- info(ipv6//1, [
		comment is 'Parses an IPv6 network address in the format XXXX.XXXX.XXXX.XXXX.XXXX.XXXX.XXXX.XXXX where each X is a hexadecimal digit.'
	]).

	:- uses(number_grammars(_Format_), [
		natural//1, dot//1, hex_digit//1
	]).

	ipv4([O3,O2,O1,O0]) -->
		octet(O3), dot(_), octet(O2), dot(_), octet(O1), dot(_), octet(O0).

	octet(Octet) -->
		natural(Octet), {0 =< Octet, Octet =< 255}.

	ipv6([H7,H6,H5,H4,H3,H2,H1,H0]) -->
		group(H7), colon, group(H6), colon, group(H5), colon, group(H4), colon, group(H3), colon, group(H2), colon, group(H1), colon, group(H0).

	group(H) -->
		hex_digit(H3), hex_digit(H2), hex_digit(H1), hex_digit(H0),
		{_Format_ == chars -> number_chars(H, ['0', 'x', H3, H2, H1, H0]); number_codes(H, [0'0, 0'x, H3, H2, H1, H0])}.

	colon -->
		colon(_Format_).

	colon(chars) -->
		[':'].
	colon(codes) -->
		[0':].

:- end_object.

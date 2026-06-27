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


	cover(http_cookies(_)).

	test(valid_cookie_1_01, true) :-
		http_cookies::valid_cookie("SID=31d4d96e407aad42").

	test(valid_cookie_1_02, true) :-
		http_cookies::valid_cookie("SID=31d4d96e407aad42; lang=en-US").

	test(valid_cookie_1_03, true) :-
		http_cookies::valid_cookie("token=\"abc123\"").

	test(valid_cookie_1_04, false) :-
		http_cookies::valid_cookie("SID =31d4d96e407aad42").

	test(parse_cookie_2_01, deterministic(Pairs == ["SID"-"31d4d96e407aad42", "lang"-"en-US"])) :-
		http_cookies::parse_cookie("SID=31d4d96e407aad42; lang=en-US", Pairs).

	test(parse_cookie_2_02, deterministic(Pairs == ["flag"-""])) :-
		http_cookies::parse_cookie("flag=", Pairs).

	test(generate_cookie_2_01, deterministic(Cookie == "SID=31d4d96e407aad42; lang=en-US")) :-
		http_cookies::generate_cookie(["SID"-"31d4d96e407aad42", "lang"-"en-US"], Cookie).

	test(valid_set_cookie_1_01, true) :-
		http_cookies::valid_set_cookie("SID=31d4d96e407aad42; Path=/; Secure; HttpOnly").

	test(valid_set_cookie_1_02, true) :-
		http_cookies::valid_set_cookie("SID=31d4d96e407aad42; Expires=Wed, 09 Jun 2021 10:18:14 GMT; Max-Age=3600; Domain=example.com; Path=/docs; Secure; HttpOnly; Priority=High").

	test(valid_set_cookie_1_03, false) :-
		http_cookies::valid_set_cookie("SID=31d4d96e407aad42; Max-Age=-1").

	test(parse_set_cookie_2_01, deterministic((Name == "SID", Value == "31d4d96e407aad42", Attributes == [path-("/"), secure-true, http_only-true]))) :-
		http_cookies::parse_set_cookie("SID=31d4d96e407aad42; Path=/; Secure; HttpOnly", Name, Value, Attributes).

	test(parse_set_cookie_2_02, deterministic((Name == "SID", Value == "31d4d96e407aad42", Attributes == [expires-date_time(2021, 6, 9, 10, 18, 14), max_age-3600, domain-"example.com", path-"/docs", secure-true, http_only-true, priority-high]))) :-
		http_cookies::parse_set_cookie("SID=31d4d96e407aad42; Expires=Wed, 09 Jun 2021 10:18:14 GMT; Max-Age=3600; Domain=example.com; Path=/docs; Secure; HttpOnly; Priority=High", Name, Value, Attributes).

	test(parse_set_cookie_2_03, deterministic((Name == "SID", Value == "31d4d96e407aad42", Attributes == [secure-true, http_only-true, path-("/")]))) :-
		http_cookies::parse_set_cookie("SID=31d4d96e407aad42; secure; httponly; path=/", Name, Value, Attributes).

	test(parse_set_cookie_2_04, deterministic((Name == "SID", Value == "31d4d96e407aad42", Attributes == [same_site-none, partitioned-true, path-("/docs"), secure-true]))) :-
		http_cookies::parse_set_cookie("SID=31d4d96e407aad42; SameSite=None; Partitioned; Path=/docs; Secure", Name, Value, Attributes).

	test(parse_set_cookie_2_05, deterministic((Name == "SID", Value == "31d4d96e407aad42", Attributes == [http_only-true, same_site-none, secure-true, partitioned-true]))) :-
		http_cookies::parse_set_cookie("SID=31d4d96e407aad42; HttpOnly=1; SameSite=None; Secure=1; Partitioned=1", Name, Value, Attributes).

	test(generate_set_cookie_2_01, deterministic(SetCookie == "SID=31d4d96e407aad42; Expires=Wed, 09 Jun 2021 10:18:14 GMT; Max-Age=3600; Domain=example.com; Path=/docs; Secure; HttpOnly; Priority=High")) :-
		http_cookies::generate_set_cookie(
			"SID",
			"31d4d96e407aad42",
			[
				expires-date_time(2021, 6, 9, 10, 18, 14),
				max_age-3600,
				domain-"example.com",
				path-"/docs",
				secure-true,
				http_only-true,
				priority-high
			],
			SetCookie
		).

	test(generate_set_cookie_2_02, deterministic(SetCookie == "flag=; Path=/")) :-
		http_cookies::generate_set_cookie("flag", "", [path-("/")], SetCookie).

	test(normalize_cookie_attributes_2_01, deterministic(NormalizedAttributes == [expires-date_time(2021, 6, 9, 10, 18, 14), secure-true, priority-high])) :-
		http_cookies::normalize_cookie_attributes([expires-"Wed, 09 Jun 2021 10:18:14 GMT", secure-true, priority-high], NormalizedAttributes).

	test(cookie_attribute_present_2_01, true) :-
		http_cookies::cookie_attribute_present([path-("/"), secure-true, http_only-true], secure).

	test(cookie_attribute_value_3_01, deterministic(DateTime == date_time(2021, 6, 9, 10, 18, 14))) :-
		http_cookies::cookie_attribute_value([expires-"Wed, 09 Jun 2021 10:18:14 GMT", secure-true], expires, DateTime).

	test(cookie_attribute_value_4_01, deterministic(Value == false)) :-
		http_cookies::cookie_attribute_value([path-("/")], secure, false, Value).

	test(cookie_expiry_2_01, deterministic(Expiry == max_age(3600))) :-
		http_cookies::cookie_expiry([expires-date_time(2021, 6, 9, 10, 18, 14), max_age-3600], Expiry).

	test(cookie_expiry_3_01, deterministic(Expiry == expires(160))) :-
		http_cookies::cookie_expiry([max_age-60], 100, Expiry).

	test(cookie_deletion_3_01, deterministic(SetCookie == set_cookie("SID", "", [domain-"example.com", path-"/docs", secure-true, http_only-true, same_site-none, partitioned-true, expires-date_time(1970, 1, 1, 0, 0, 0), max_age-0]))) :-
		http_cookies::cookie_deletion("SID", [domain-"example.com", path-"/docs", secure-true, http_only-true, same_site-none, partitioned-true, priority-high], SetCookie).

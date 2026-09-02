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


	cover(uri_template(_)).

	% valid/1 tests

	test(uri_template_valid_1_01, deterministic) :-
		uri_template::valid("https://example.com/~fred/").

	test(uri_template_valid_1_02, deterministic) :-
		uri_template::valid("http://example.com/search{?q,lang}").

	test(uri_template_valid_1_03, deterministic) :-
		uri_template::valid("{var}{+path}{#fragment}{.ext}{/segments*}{;x,y}{?q}{&page}").

	test(uri_template_valid_1_04, deterministic) :-
		uri_template::valid("{var:9999}{first%20name}").

	test(uri_template_valid_1_05, false) :-
		uri_template::valid("{}").

	test(uri_template_valid_1_06, false) :-
		uri_template::valid("{var").

	test(uri_template_valid_1_07, false) :-
		uri_template::valid("{{var}}").

	test(uri_template_valid_1_08, false) :-
		uri_template::valid("{=var}").

	test(uri_template_valid_1_09, false) :-
		uri_template::valid("{var:0}").

	test(uri_template_valid_1_10, false) :-
		uri_template::valid("{var:10000}").

	test(uri_template_valid_1_11, false) :-
		uri_template::valid("{var:*}").

	test(uri_template_valid_1_12, false) :-
		uri_template::valid("{a..b}").

	test(uri_template_valid_1_13, false) :-
		uri_template::valid("{bad%2G}").

	test(uri_template_valid_1_14, false) :-
		uri_template::valid("'").

	% variables/2 tests

	test(uri_template_variables_2_01, deterministic(Variables == [])) :-
		uri_template::variables("https://example.com/", Variables).

	test(uri_template_variables_2_02, deterministic(Variables == ["x", "y", "list"])) :-
		uri_template::variables("{x}{?y,x,list*}", Variables).

	test(uri_template_variables_2_03, deterministic(Variables == ["Var", "var", "first%20name"])) :-
		uri_template::variables("{Var,var}{first%20name:3}", Variables).

	test(uri_template_variables_2_04, false) :-
		uri_template::variables("{x,,y}", _).

	% expand/3 tests from RFC 6570

	test(uri_template_expand_3_01, deterministic(Expansion == "value")) :-
		uri_template::expand("{var}", ["var"-string("value")], Expansion).

	test(uri_template_expand_3_02, deterministic(Expansion == "Hello%20World%21")) :-
		uri_template::expand("{hello}", ["hello"-string("Hello World!")], Expansion).

	test(uri_template_expand_3_03, deterministic(Expansion == "/foo/bar")) :-
		uri_template::expand("{+path}", ["path"-string("/foo/bar")], Expansion).

	test(uri_template_expand_3_04, deterministic(Expansion == "#/foo/bar,1024")) :-
		uri_template::expand("{#path,x}", ["path"-string("/foo/bar"), "x"-string("1024")], Expansion).

	test(uri_template_expand_3_05, deterministic(Expansion == "X.value")) :-
		uri_template::expand("X{.var}", ["var"-string("value")], Expansion).

	test(uri_template_expand_3_06, deterministic(Expansion == "/value/1024")) :-
		uri_template::expand("{/var,x}", ["var"-string("value"), "x"-string("1024")], Expansion).

	test(uri_template_expand_3_07, deterministic(Expansion == ";x=1024;y=768;empty")) :-
		uri_template::expand("{;x,y,empty}", ["x"-string("1024"), "y"-string("768"), "empty"-string("")], Expansion).

	test(uri_template_expand_3_08, deterministic(Expansion == "?x=1024&y=768&empty=")) :-
		uri_template::expand("{?x,y,empty}", ["x"-string("1024"), "y"-string("768"), "empty"-string("")], Expansion).

	test(uri_template_expand_3_09, deterministic(Expansion == "?fixed=yes&x=1024")) :-
		uri_template::expand("?fixed=yes{&x}", ["x"-string("1024")], Expansion).

	test(uri_template_expand_3_10, deterministic(Expansion == "val")) :-
		uri_template::expand("{var:3}", ["var"-string("value")], Expansion).

	test(uri_template_expand_3_11, deterministic(Expansion == "/red,green,blue")) :-
		uri_template::expand("{/list}", ["list"-list(["red", "green", "blue"])], Expansion).

	test(uri_template_expand_3_12, deterministic(Expansion == "/red/green/blue")) :-
		uri_template::expand("{/list*}", ["list"-list(["red", "green", "blue"])], Expansion).

	test(uri_template_expand_3_13, deterministic(Expansion == "?list=red&list=green&list=blue")) :-
		uri_template::expand("{?list*}", ["list"-list(["red", "green", "blue"])], Expansion).

	test(uri_template_expand_3_14, deterministic(Expansion == "semi,%3B,dot,.,comma,%2C")) :-
		uri_template::expand("{keys}", ["keys"-assoc(['-'("semi", ";"), '-'("dot", "."), '-'("comma", ",")])], Expansion).

	test(uri_template_expand_3_15, deterministic(Expansion == "?semi=%3B&dot=.&comma=%2C")) :-
		uri_template::expand("{?keys*}", ["keys"-assoc(['-'("semi", ";"), '-'("dot", "."), '-'("comma", ",")])], Expansion).

	test(uri_template_expand_3_16, deterministic(Expansion == "OX")) :-
		uri_template::expand("O{undef}X", ["undef"-undefined], Expansion).

	test(uri_template_expand_3_17, deterministic(Expansion == "OX")) :-
		uri_template::expand("O{missing}X", [], Expansion).

	test(uri_template_expand_3_18, deterministic(Expansion == "foo#")) :-
		uri_template::expand("foo{#empty}", ["empty"-string("")], Expansion).

	test(uri_template_expand_3_20, deterministic(Expansion == "%252F/%2F")) :-
		uri_template::expand("{value}/{+value}", ["value"-string("%2F")], Expansion).

	test(uri_template_expand_3_21, deterministic(Expansion == "")) :-
		uri_template::expand("{/values*}", ["values"-list([])], Expansion).

	test(uri_template_expand_3_22, deterministic(Expansion == "?a=1&c=")) :-
		uri_template::expand("{?values*}", ["values"-assoc(["a"-"1", "b"-undefined, "c"-""])], Expansion).

	test(uri_template_expand_3_23, false) :-
		uri_template::expand("{values:2}", ["values"-list(["one"])], _).

	test(uri_template_expand_3_24, false) :-
		uri_template::expand("{x}", ["x"-string("one"), "x"-string("two")], _).

	test(uri_template_expand_3_25, false) :-
		uri_template::expand("{x}", ["x"-"value"], _).

	test(uri_template_expand_3_26, deterministic(Expansion == "a%20b")) :-
		uri_template::expand("a%20b", [], Expansion).

	test(uri_template_expand_3_27, deterministic(Expansion == "?first%20name=value")) :-
		uri_template::expand("{?first%20name}", ["first%20name"-string("value")], Expansion).

	test(uri_template_expand_3_30, deterministic(Expansion == "?city=Newport%20Beach&region.code=CA")) :-
		uri_template::expand(
			"{?address*}",
			["address"-structure(["city"-"Newport Beach", "region"-structure(["code"-"CA"])])],
			Expansion
		).

	% expand/4 diagnostic tests

	test(uri_template_expand_4_01, deterministic((Expansion == "a{bad!}bok", Errors == [error(1, malformed_expression)]))) :-
		uri_template::expand("a{bad!}b{x}", ["x"-string("ok")], Expansion, Errors).

	test(uri_template_expand_4_02, deterministic((Expansion == "ok/{bad", Errors == [error(4, unterminated_expression)]))) :-
		uri_template::expand("{x}/{bad", ["x"-string("ok")], Expansion, Errors).

	test(uri_template_expand_4_03, deterministic((Expansion == "a b{x}", Errors == [error(1, invalid_literal)]))) :-
		uri_template::expand("a b{x}", ["x"-string("ok")], Expansion, Errors).

	test(uri_template_expand_4_04, deterministic((Expansion == "ok", Errors == []))) :-
		uri_template::expand("{x}", ["x"-string("ok")], Expansion, Errors).

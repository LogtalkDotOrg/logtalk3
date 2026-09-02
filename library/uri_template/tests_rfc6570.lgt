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


	% RFC 6570 section 1.2 examples not repeated verbatim in section 3.2

	test(uri_template_rfc_6570_1_2_01, deterministic) :- rfc_6570_expands("X{#var}", "X#value").
	test(uri_template_rfc_6570_1_2_02, deterministic) :- rfc_6570_expands("X{#hello}", "X#Hello%20World!").
	test(uri_template_rfc_6570_1_2_03, deterministic) :- rfc_6570_expands("map?{x,y}", "map?1024,768").
	test(uri_template_rfc_6570_1_2_04, deterministic) :- rfc_6570_expands("X{.x,y}", "X.1024.768").

	% RFC 6570 section 3.2.1

	test(uri_template_rfc_6570_3_2_1_01, deterministic) :- rfc_6570_expands("{count}", "one,two,three").
	test(uri_template_rfc_6570_3_2_1_02, deterministic) :- rfc_6570_expands("{count*}", "one,two,three").
	test(uri_template_rfc_6570_3_2_1_03, deterministic) :- rfc_6570_expands("{/count}", "/one,two,three").
	test(uri_template_rfc_6570_3_2_1_04, deterministic) :- rfc_6570_expands("{/count*}", "/one/two/three").
	test(uri_template_rfc_6570_3_2_1_05, deterministic) :- rfc_6570_expands("{;count}", ";count=one,two,three").
	test(uri_template_rfc_6570_3_2_1_06, deterministic) :- rfc_6570_expands("{;count*}", ";count=one;count=two;count=three").
	test(uri_template_rfc_6570_3_2_1_07, deterministic) :- rfc_6570_expands("{?count}", "?count=one,two,three").
	test(uri_template_rfc_6570_3_2_1_08, deterministic) :- rfc_6570_expands("{?count*}", "?count=one&count=two&count=three").
	test(uri_template_rfc_6570_3_2_1_09, deterministic) :- rfc_6570_expands("{&count*}", "&count=one&count=two&count=three").

	% RFC 6570 section 3.2.2

	test(uri_template_rfc_6570_3_2_2_01, deterministic) :- rfc_6570_expands("{var}", "value").
	test(uri_template_rfc_6570_3_2_2_02, deterministic) :- rfc_6570_expands("{hello}", "Hello%20World%21").
	test(uri_template_rfc_6570_3_2_2_03, deterministic) :- rfc_6570_expands("{half}", "50%25").
	test(uri_template_rfc_6570_3_2_2_04, deterministic) :- rfc_6570_expands("O{empty}X", "OX").
	test(uri_template_rfc_6570_3_2_2_05, deterministic) :- rfc_6570_expands("O{undef}X", "OX").
	test(uri_template_rfc_6570_3_2_2_06, deterministic) :- rfc_6570_expands("{x,y}", "1024,768").
	test(uri_template_rfc_6570_3_2_2_07, deterministic) :- rfc_6570_expands("{x,hello,y}", "1024,Hello%20World%21,768").
	test(uri_template_rfc_6570_3_2_2_08, deterministic) :- rfc_6570_expands("?{x,empty}", "?1024,").
	test(uri_template_rfc_6570_3_2_2_09, deterministic) :- rfc_6570_expands("?{x,undef}", "?1024").
	test(uri_template_rfc_6570_3_2_2_10, deterministic) :- rfc_6570_expands("?{undef,y}", "?768").
	test(uri_template_rfc_6570_3_2_2_11, deterministic) :- rfc_6570_expands("{var:3}", "val").
	test(uri_template_rfc_6570_3_2_2_12, deterministic) :- rfc_6570_expands("{var:30}", "value").
	test(uri_template_rfc_6570_3_2_2_13, deterministic) :- rfc_6570_expands("{list}", "red,green,blue").
	test(uri_template_rfc_6570_3_2_2_14, deterministic) :- rfc_6570_expands("{list*}", "red,green,blue").
	test(uri_template_rfc_6570_3_2_2_15, deterministic) :- rfc_6570_expands("{keys}", "semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_2_16, deterministic) :- rfc_6570_expands("{keys*}", "semi=%3B,dot=.,comma=%2C").

	% RFC 6570 section 3.2.3

	test(uri_template_rfc_6570_3_2_3_01, deterministic) :- rfc_6570_expands("{+var}", "value").
	test(uri_template_rfc_6570_3_2_3_02, deterministic) :- rfc_6570_expands("{+hello}", "Hello%20World!").
	test(uri_template_rfc_6570_3_2_3_03, deterministic) :- rfc_6570_expands("{+half}", "50%25").
	test(uri_template_rfc_6570_3_2_3_04, deterministic) :- rfc_6570_expands("{base}index", "http%3A%2F%2Fexample.com%2Fhome%2Findex").
	test(uri_template_rfc_6570_3_2_3_05, deterministic) :- rfc_6570_expands("{+base}index", "http://example.com/home/index").
	test(uri_template_rfc_6570_3_2_3_06, deterministic) :- rfc_6570_expands("O{+empty}X", "OX").
	test(uri_template_rfc_6570_3_2_3_07, deterministic) :- rfc_6570_expands("O{+undef}X", "OX").
	test(uri_template_rfc_6570_3_2_3_08, deterministic) :- rfc_6570_expands("{+path}/here", "/foo/bar/here").
	test(uri_template_rfc_6570_3_2_3_09, deterministic) :- rfc_6570_expands("here?ref={+path}", "here?ref=/foo/bar").
	test(uri_template_rfc_6570_3_2_3_10, deterministic) :- rfc_6570_expands("up{+path}{var}/here", "up/foo/barvalue/here").
	test(uri_template_rfc_6570_3_2_3_11, deterministic) :- rfc_6570_expands("{+x,hello,y}", "1024,Hello%20World!,768").
	test(uri_template_rfc_6570_3_2_3_12, deterministic) :- rfc_6570_expands("{+path,x}/here", "/foo/bar,1024/here").
	test(uri_template_rfc_6570_3_2_3_13, deterministic) :- rfc_6570_expands("{+path:6}/here", "/foo/b/here").
	test(uri_template_rfc_6570_3_2_3_14, deterministic) :- rfc_6570_expands("{+list}", "red,green,blue").
	test(uri_template_rfc_6570_3_2_3_15, deterministic) :- rfc_6570_expands("{+list*}", "red,green,blue").
	test(uri_template_rfc_6570_3_2_3_16, deterministic) :- rfc_6570_expands("{+keys}", "semi,;,dot,.,comma,,").
	test(uri_template_rfc_6570_3_2_3_17, deterministic) :- rfc_6570_expands("{+keys*}", "semi=;,dot=.,comma=,").

	% RFC 6570 section 3.2.4

	test(uri_template_rfc_6570_3_2_4_01, deterministic) :- rfc_6570_expands("{#var}", "#value").
	test(uri_template_rfc_6570_3_2_4_02, deterministic) :- rfc_6570_expands("{#hello}", "#Hello%20World!").
	test(uri_template_rfc_6570_3_2_4_03, deterministic) :- rfc_6570_expands("{#half}", "#50%25").
	test(uri_template_rfc_6570_3_2_4_04, deterministic) :- rfc_6570_expands("foo{#empty}", "foo#").
	test(uri_template_rfc_6570_3_2_4_05, deterministic) :- rfc_6570_expands("foo{#undef}", "foo").
	test(uri_template_rfc_6570_3_2_4_06, deterministic) :- rfc_6570_expands("{#x,hello,y}", "#1024,Hello%20World!,768").
	test(uri_template_rfc_6570_3_2_4_07, deterministic) :- rfc_6570_expands("{#path,x}/here", "#/foo/bar,1024/here").
	test(uri_template_rfc_6570_3_2_4_08, deterministic) :- rfc_6570_expands("{#path:6}/here", "#/foo/b/here").
	test(uri_template_rfc_6570_3_2_4_09, deterministic) :- rfc_6570_expands("{#list}", "#red,green,blue").
	test(uri_template_rfc_6570_3_2_4_10, deterministic) :- rfc_6570_expands("{#list*}", "#red,green,blue").
	test(uri_template_rfc_6570_3_2_4_11, deterministic) :- rfc_6570_expands("{#keys}", "#semi,;,dot,.,comma,,").
	test(uri_template_rfc_6570_3_2_4_12, deterministic) :- rfc_6570_expands("{#keys*}", "#semi=;,dot=.,comma=,").

	% RFC 6570 section 3.2.5

	test(uri_template_rfc_6570_3_2_5_01, deterministic) :- rfc_6570_expands("{.who}", ".fred").
	test(uri_template_rfc_6570_3_2_5_02, deterministic) :- rfc_6570_expands("{.who,who}", ".fred.fred").
	test(uri_template_rfc_6570_3_2_5_03, deterministic) :- rfc_6570_expands("{.half,who}", ".50%25.fred").
	test(uri_template_rfc_6570_3_2_5_04, deterministic) :- rfc_6570_expands("www{.dom*}", "www.example.com").
	test(uri_template_rfc_6570_3_2_5_05, deterministic) :- rfc_6570_expands("X{.var}", "X.value").
	test(uri_template_rfc_6570_3_2_5_06, deterministic) :- rfc_6570_expands("X{.empty}", "X.").
	test(uri_template_rfc_6570_3_2_5_07, deterministic) :- rfc_6570_expands("X{.undef}", "X").
	test(uri_template_rfc_6570_3_2_5_08, deterministic) :- rfc_6570_expands("X{.var:3}", "X.val").
	test(uri_template_rfc_6570_3_2_5_09, deterministic) :- rfc_6570_expands("X{.list}", "X.red,green,blue").
	test(uri_template_rfc_6570_3_2_5_10, deterministic) :- rfc_6570_expands("X{.list*}", "X.red.green.blue").
	test(uri_template_rfc_6570_3_2_5_11, deterministic) :- rfc_6570_expands("X{.keys}", "X.semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_5_12, deterministic) :- rfc_6570_expands("X{.keys*}", "X.semi=%3B.dot=..comma=%2C").
	test(uri_template_rfc_6570_3_2_5_13, deterministic) :- rfc_6570_expands("X{.empty_keys}", "X").
	test(uri_template_rfc_6570_3_2_5_14, deterministic) :- rfc_6570_expands("X{.empty_keys*}", "X").

	% RFC 6570 section 3.2.6

	test(uri_template_rfc_6570_3_2_6_01, deterministic) :- rfc_6570_expands("{/who}", "/fred").
	test(uri_template_rfc_6570_3_2_6_02, deterministic) :- rfc_6570_expands("{/who,who}", "/fred/fred").
	test(uri_template_rfc_6570_3_2_6_03, deterministic) :- rfc_6570_expands("{/half,who}", "/50%25/fred").
	test(uri_template_rfc_6570_3_2_6_04, deterministic) :- rfc_6570_expands("{/who,dub}", "/fred/me%2Ftoo").
	test(uri_template_rfc_6570_3_2_6_05, deterministic) :- rfc_6570_expands("{/var}", "/value").
	test(uri_template_rfc_6570_3_2_6_06, deterministic) :- rfc_6570_expands("{/var,empty}", "/value/").
	test(uri_template_rfc_6570_3_2_6_07, deterministic) :- rfc_6570_expands("{/var,undef}", "/value").
	test(uri_template_rfc_6570_3_2_6_08, deterministic) :- rfc_6570_expands("{/var,x}/here", "/value/1024/here").
	test(uri_template_rfc_6570_3_2_6_09, deterministic) :- rfc_6570_expands("{/var:1,var}", "/v/value").
	test(uri_template_rfc_6570_3_2_6_10, deterministic) :- rfc_6570_expands("{/list}", "/red,green,blue").
	test(uri_template_rfc_6570_3_2_6_11, deterministic) :- rfc_6570_expands("{/list*}", "/red/green/blue").
	test(uri_template_rfc_6570_3_2_6_12, deterministic) :- rfc_6570_expands("{/list*,path:4}", "/red/green/blue/%2Ffoo").
	test(uri_template_rfc_6570_3_2_6_13, deterministic) :- rfc_6570_expands("{/keys}", "/semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_6_14, deterministic) :- rfc_6570_expands("{/keys*}", "/semi=%3B/dot=./comma=%2C").

	% RFC 6570 section 3.2.7

	test(uri_template_rfc_6570_3_2_7_01, deterministic) :- rfc_6570_expands("{;who}", ";who=fred").
	test(uri_template_rfc_6570_3_2_7_02, deterministic) :- rfc_6570_expands("{;half}", ";half=50%25").
	test(uri_template_rfc_6570_3_2_7_03, deterministic) :- rfc_6570_expands("{;empty}", ";empty").
	test(uri_template_rfc_6570_3_2_7_04, deterministic) :- rfc_6570_expands("{;v,empty,who}", ";v=6;empty;who=fred").
	test(uri_template_rfc_6570_3_2_7_05, deterministic) :- rfc_6570_expands("{;v,bar,who}", ";v=6;who=fred").
	test(uri_template_rfc_6570_3_2_7_06, deterministic) :- rfc_6570_expands("{;x,y}", ";x=1024;y=768").
	test(uri_template_rfc_6570_3_2_7_07, deterministic) :- rfc_6570_expands("{;x,y,empty}", ";x=1024;y=768;empty").
	test(uri_template_rfc_6570_3_2_7_08, deterministic) :- rfc_6570_expands("{;x,y,undef}", ";x=1024;y=768").
	test(uri_template_rfc_6570_3_2_7_09, deterministic) :- rfc_6570_expands("{;hello:5}", ";hello=Hello").
	test(uri_template_rfc_6570_3_2_7_10, deterministic) :- rfc_6570_expands("{;list}", ";list=red,green,blue").
	test(uri_template_rfc_6570_3_2_7_11, deterministic) :- rfc_6570_expands("{;list*}", ";list=red;list=green;list=blue").
	test(uri_template_rfc_6570_3_2_7_12, deterministic) :- rfc_6570_expands("{;keys}", ";keys=semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_7_13, deterministic) :- rfc_6570_expands("{;keys*}", ";semi=%3B;dot=.;comma=%2C").

	% RFC 6570 section 3.2.8

	test(uri_template_rfc_6570_3_2_8_01, deterministic) :- rfc_6570_expands("{?who}", "?who=fred").
	test(uri_template_rfc_6570_3_2_8_02, deterministic) :- rfc_6570_expands("{?half}", "?half=50%25").
	test(uri_template_rfc_6570_3_2_8_03, deterministic) :- rfc_6570_expands("{?x,y}", "?x=1024&y=768").
	test(uri_template_rfc_6570_3_2_8_04, deterministic) :- rfc_6570_expands("{?x,y,empty}", "?x=1024&y=768&empty=").
	test(uri_template_rfc_6570_3_2_8_05, deterministic) :- rfc_6570_expands("{?x,y,undef}", "?x=1024&y=768").
	test(uri_template_rfc_6570_3_2_8_06, deterministic) :- rfc_6570_expands("{?var:3}", "?var=val").
	test(uri_template_rfc_6570_3_2_8_07, deterministic) :- rfc_6570_expands("{?list}", "?list=red,green,blue").
	test(uri_template_rfc_6570_3_2_8_08, deterministic) :- rfc_6570_expands("{?list*}", "?list=red&list=green&list=blue").
	test(uri_template_rfc_6570_3_2_8_09, deterministic) :- rfc_6570_expands("{?keys}", "?keys=semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_8_10, deterministic) :- rfc_6570_expands("{?keys*}", "?semi=%3B&dot=.&comma=%2C").

	% RFC 6570 section 3.2.9

	test(uri_template_rfc_6570_3_2_9_01, deterministic) :- rfc_6570_expands("{&who}", "&who=fred").
	test(uri_template_rfc_6570_3_2_9_02, deterministic) :- rfc_6570_expands("{&half}", "&half=50%25").
	test(uri_template_rfc_6570_3_2_9_03, deterministic) :- rfc_6570_expands("?fixed=yes{&x}", "?fixed=yes&x=1024").
	test(uri_template_rfc_6570_3_2_9_04, deterministic) :- rfc_6570_expands("{&x,y,empty}", "&x=1024&y=768&empty=").
	test(uri_template_rfc_6570_3_2_9_05, deterministic) :- rfc_6570_expands("{&x,y,undef}", "&x=1024&y=768").
	test(uri_template_rfc_6570_3_2_9_06, deterministic) :- rfc_6570_expands("{&var:3}", "&var=val").
	test(uri_template_rfc_6570_3_2_9_07, deterministic) :- rfc_6570_expands("{&list}", "&list=red,green,blue").
	test(uri_template_rfc_6570_3_2_9_08, deterministic) :- rfc_6570_expands("{&list*}", "&list=red&list=green&list=blue").
	test(uri_template_rfc_6570_3_2_9_09, deterministic) :- rfc_6570_expands("{&keys}", "&keys=semi,%3B,dot,.,comma,%2C").
	test(uri_template_rfc_6570_3_2_9_10, deterministic) :- rfc_6570_expands("{&keys*}", "&semi=%3B&dot=.&comma=%2C").

	% auxiliary predicates

	rfc_6570_expands(Template, Expected) :-
		rfc_6570_bindings(Bindings),
		uri_template::expand(Template, Bindings, Expansion),
		Expansion == Expected.

	rfc_6570_bindings([
		"count"-list(["one", "two", "three"]),
		"dom"-list(["example", "com"]),
		"dub"-string("me/too"),
		"hello"-string("Hello World!"),
		"half"-string("50%"),
		"var"-string("value"),
		"who"-string("fred"),
		"base"-string("http://example.com/home/"),
		"path"-string("/foo/bar"),
		"list"-list(["red", "green", "blue"]),
		"keys"-assoc(['-'("semi", ";"), '-'("dot", "."), '-'("comma", ",")]),
		"v"-string("6"),
		"x"-string("1024"),
		"y"-string("768"),
		"empty"-string(""),
		"empty_keys"-assoc([]),
		"undef"-undefined
	]).

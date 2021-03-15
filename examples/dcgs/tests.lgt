%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:14:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2021-03-15,
		comment is 'Unit tests for the "dcgs" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(dcgs_01, true(Solutions == [-9])) :-
		findall(Result, calculator::parse("1+2-3*4", Result), Solutions).

	test(dcgs_02, true) :-
		macaddr::valid("00:1e:4a:ef:72:8b").

	test(dcgs_03, true(Solutions == [[sos]])) :-
		findall(Message, logtalk << phrase(morse::morse(Message), "... --- ..."), Solutions).

	:- if(current_logtalk_flag(prolog_dialect, tau)).
		- test(dcgs_04, true).
	:- else.
		test(dcgs_04, true(Solutions == [[i, would, love, to, have, dinner, with, you, tonight]])) :-
			findall(Message, enigma::solve("4 96853 5683 86 4283 346637 9484 968 8664448", Message), Solutions).
	:- endif.

	test(dcgs_05, true(Result == true)) :-
		sentence::parse([the, girl, likes, the, boy], Result).

	test(dcgs_06, true(Result == false)) :-
		sentence::parse([the, girl, scares, the, boy], Result).

	test(dcgs_07, true(Trees == [s(np(d(the), n(girl)), vp(v(likes), np(d(the), n(boy))))])) :-
		findall(Tree, parsetree::parse([the, girl, likes, the, boy], Tree), Trees).

	test(dcgs_08, true(Solutions == [[frame, crank, pedal, pedal, chain, spokes, rim, hub, spokes, rim, hub]])) :-
		findall(L, bom::parts(bike, L), Solutions).

	test(dcgs_09, true(Solutions == [[spokes, rim, hub]])) :-
		findall(L,bom::parts(wheel, L), Solutions).

	test(dcgs_10, true(Solutions == [[pwd,'cd ..','ls -a']])) :-
		findall(L, shell::parse("pwd; cd ..; ls -a", L), Solutions).

	% test 11.  % complicated because of comparison of floats
	test(dcgs_11, true) :-
		tokenizer::tokens(" We owe $1,048,576.24 to Agent 007 for Version 3.14159! ", Tokens),
		Tokens = [Tok1, Tok2, Tok3, Number| Rest],
		Number =~= 1048576.24,	% Wow the error is huge (>3)
		Tok1 == we, Tok2 == owe, Tok3 == ('$'), Rest == [to,agent,7,for,version,3.14159,!].

	test(dcgs_12, true([A, B] =~= [-0.94974746830583223, 6.9497474683058318])) :-
		findall(Ending, walker::walk([n(5), e(4), s(2), nw(8), s(5), se(1), n(4)], Ending), Endings),
		Endings = [(A, B)].

	test(dcgs_13, true(Solutions == ['<word><singular>child</singular><plural>children</plural></word>'])) :-
		findall(XML, xml::convert(word(child, children), word(singular, plural), XML), Solutions).

	test(dcgs_14, true(Solutions == [word(child, children)-word(singular, plural)])) :-
		findall(Term-Interpretation, xml::convert(Term, Interpretation, '<word><singular>child</singular><plural>children</plural></word>'), Solutions).

	test(dcgs_15, true(Solutions == [[protocol(https), address([logtalk, org]), path([]), file('')]])) :-
		findall(Components, url::parse("https://logtalk.org", Components), Solutions).

	test(dcgs_16, true(Solutions == [[protocol(https), address([logtalk, org]), path(['']), file('')]])) :-
		findall(Components, url::parse("https://logtalk.org/", Components), Solutions).

	test(dcgs_17, true(Solutions == [[protocol(https), address([logtalk, org]), path([cvs]), file('')]])) :-
		findall(Components, url::parse("https://logtalk.org/cvs", Components), Solutions).

	test(dcgs_18, true(Solutions == [[protocol(https), address([logtalk, org]), path([]), file('cvs.html')]])) :-
		findall(Components, url::parse("https://logtalk.org/cvs.html", Components), Solutions).

	test(dcgs_19, true(Solutions == [[protocol(https), address([logtalk, org]), path([files,update]), file('')]])) :-
		findall(Components, url::parse("https://logtalk.org/files/update", Components), Solutions).

	:- if(current_prolog_flag(bounded, false)).
		test(dcgs_20, true) :-
			iban::valid("GB82 WEST 1234 5698 7654 32").
	:- else.
		- test(dcgs_20, true).
	:- endif.

	test(dcgs_21, true) :-
		^^suppress_text_output,
		logtalk << phrase(bypass::foo, _, _).

	% test access to the grammar rule implicit list of tokens using the call//1 built-in
	% non-terminal and lambda expressions:

	test(dcgs_22, true) :-
		logtalk << phrase([[], []]>>true, [], []).

	test(dcgs_23, true) :-
		logtalk << phrase([[], []]>>true, Input, Rest),
		Input == [], Rest == [].

	test(dcgs_24, true) :-
		logtalk << phrase([Input, Rest]>>(set::subtract(Input, Rest, [1])), [1,2,3], [2,3]).

	% three nasty examples of getting a grammar rule difference list arguments
	% as they require using variables as both lambda free and lambda parameters

	test(dcgs_25, true(Input-Rest == [1,2,3]-[2,3])) :-
		logtalk << phrase({Input,Rest}/[Input,Rest]>>true, [1,2,3], [2,3]).

	test(dcgs_26, true(Rest == [2,3])) :-
		logtalk << phrase({Rest}/[_,Rest]>>true, [1,2,3], [2,3]).

	test(dcgs_27, true(Element == 1)) :-
		logtalk << phrase({Element}/[[Element|_],_]>>true, [1,2,3], [2,3]).

	% cuts in the first argument of phrase/2-3 calls must be local and not extend outside:

	test(dcgs_28, true(Xs == [1,2,3])) :-
		findall(X, (list::member(X, [1,2,3]), logtalk << phrase(!, _)), Xs).

	test(dcgs_29, true(Xs == [1,2,3])) :-
		findall(X, (list::member(X, [1,2,3]), logtalk << phrase(!, _, _)), Xs).

	test(dcgs_30, true) :-
		^^suppress_text_output,
		client::print.

	test(dcgs_31, true(Successors == [2,3,4])) :-
		client::successors([1,2,3], Successors).

	% lambda expressions in grammar rules

	test(dcgs_32, true(Duplicates == [a,a,b,b,c,c])) :-
		logtalk << phrase(lambdas::aa(Duplicates), [a,b,c]).

	test(dcgs_33, true(Singletons == [a,b,c])) :-
		logtalk << phrase(lambdas::aa([a,a,b,b,c,c]), Singletons).

	test(dcgs_34, true(Duplicates == [a,a,b,b,c,c])) :-
		logtalk << phrase(lambdas::bb(Duplicates), [a,b,c]).

	test(dcgs_35, true(Singletons == [a,b,c])) :-
		logtalk << phrase(lambdas::bb([a,a,b,b,c,c]), Singletons).

	test(dcgs_36, true(Copy == [1, 2, 3, 4, 5])) :-
		^^suppress_text_output,
		debug::copy([1,2,3,4,5], Copy).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2020-06-03,
		comment is 'Unit tests for the "loops" library.'
	]).

	cover(loop).

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

	% whiledo/2 tests

	test(loops_whiledo_2_01, true(Assertion)) :-
		^^set_text_input(file, '123'),
		^^set_text_output(''),
		loop::whiledo(
			\+ peek_char(file, end_of_file),
			(get_char(file, Element), write(Element))
		),
		^^text_output_assertion('123', Assertion).

	% dowhile/2 tests

	test(loops_dowhile_2_01, true(Assertion)) :-
		^^set_text_input(file, '123'),
		^^set_text_output(''),
		loop::dowhile(
			(get_char(file, Element), write(Element)),
			\+ peek_char(file, end_of_file)
		),
		^^text_output_assertion('123', Assertion).

	% foreach/3 tests

	test(loops_foreach_3_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::foreach(Element, [1,2,3], write(Element)),
		^^text_output_assertion('123', Assertion).

	% foreach/4 tests

	test(loops_foreach_4_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::foreach(Element, Index, [a,b,c], (write(Index-Element), write('|'))),
		^^text_output_assertion('1-a|2-b|3-c|', Assertion).

	% forto/3 tests

	test(loops_forto_3_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::forto(1, 5, write('*')),
		^^text_output_assertion('*****', Assertion).

	% forto/4 tests

	test(loops_forto_4_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::forto(Count, 1, 5, write(Count)),
		^^text_output_assertion('12345', Assertion).

	% forto/5 tests

	test(loops_forto_5_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::forto(Count, 1, 7, 2, write(Count)),
		^^text_output_assertion('1357', Assertion).

	% fordownto/3 tests

	test(loops_fordownto_3_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::fordownto(5, 1, write('*')),
		^^text_output_assertion('*****', Assertion).

	% fordownto/4 tests

	test(loops_fordownto_4_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::fordownto(Count, 5, 1, write(Count)),
		^^text_output_assertion('54321', Assertion).

	% fordownto/5 tests

	test(loops_fordownto_5_01, true(Assertion)) :-
		^^set_text_output(''),
		loop::fordownto(Count, 7, 1, 2, write(Count)),
		^^text_output_assertion('7531', Assertion).

:- end_object.

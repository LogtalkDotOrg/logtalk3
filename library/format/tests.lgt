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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-11-12,
		comment is 'Unit tests for the "format" library.'
	]).

	:- uses(format, [
		format/3, format/2
	]).

	cover(format).

	test(format_format_2_01, true(Assertion)) :-
		^^set_text_output(''),
		format('~w~n', [foo]),
		^^text_output_assertion('foo\n', Assertion).

	test(format_format_3_01, true(Assertion)) :-
		^^set_text_output(out, ''),
		format(out, '~w~n', [bar]),
		^^text_output_assertion(out, 'bar\n', Assertion).

	cleanup :-
		^^clean_text_output.

:- end_object.

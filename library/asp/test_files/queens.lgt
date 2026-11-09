%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017 Roland Kaminski
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: MIT
%
%  Logtalk adaptation of the queens problem from:
%  https://github.com/potassco/clingo/blob/master/examples/gringo/queens/queens1.lp
%  Uses row choices and attack constraints instead of propagation auxiliaries.
%
%  The MIT License (MIT)
%
%  Copyright 2017 Roland Kaminski
%
%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%  this software and associated documentation files (the "Software"), to deal in
%  the Software without restriction, including without limitation the rights to
%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%  of the Software, and to permit persons to whom the Software is furnished to do
%  so, subject to the following conditions:
%
%  The above copyright notice and this permission notice shall be included in all
%  copies or substantial portions of the Software.
%
%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%  SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(asp_queens,
	implements(asp_problem_protocol)).

	:- const(n, 10).

	square(interval(1, n)).

	choice(1, [conditional(q(Row, Column), square(Column))], 1) :-
		square(Row).

	:- constraint((
		q(Row, Column),
		q(OtherRow, Column),
		Row < OtherRow
	)).
	:- constraint((
		q(Row, Column),
		q(OtherRow, OtherColumn),
		Row < OtherRow,
		abs(Row-OtherRow) = abs(Column-OtherColumn)
	)).

	:- show(q/2).

:- end_object.

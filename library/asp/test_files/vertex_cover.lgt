%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017 Roland Kaminski
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: MIT
%
%  Logtalk adaptation of:
%  https://github.com/potassco/clingo/blob/master/examples/clingo/cover/cover.lp
%  Python callback inputs replaced with Logtalk facts for the path a-b-c.
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


:- object(asp_vertex_cover,
	implements(asp_problem_protocol)).

	vertex(a).
	vertex(b).
	vertex(c).
	edge(a, b).
	edge(b, c).

	choice(none, [conditional(cover(Vertex), vertex(Vertex))], none).

	:- constraint((
		edge(From, To),
		not(cover(From)),
		not(cover(To))
	)).

	:- minimize([objective(1, 0, [Vertex], cover(Vertex))]).
	:- show(cover/1).

:- end_object.

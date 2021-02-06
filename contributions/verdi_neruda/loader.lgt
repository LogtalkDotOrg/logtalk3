%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright (c) 2010, Victor Lagerkvist
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- if(current_logtalk_flag(prolog_dialect, sictus)).
	:- set_prolog_flag(informational, off).
:- endif.


:- initialization((
	logtalk_load([
		types(loader),
		heaps(loader),
		queues(loader),
		meta(loader),
		random(loader)
	],
	[
		report(warnings)
	]),
	logtalk_load(counter, [report(warnings)]),
	logtalk_load(magic, [report(warnings)]),
	logtalk_load(flatting, [report(warnings)]),
	logtalk_load(debug_expansion, [report(warnings)]),
	logtalk_load(rule_expansion, [report(warnings)]),
	logtalk_load(magic_expansion, [report(warnings)]),
	logtalk_load(shell_expansion, [report(warnings)]),
	logtalk_load(heuristic_expansion, [report(warnings)]),
	logtalk_load(benchmark_generators, [report(warnings)]),
	logtalk_load(databasep, [report(warnings)]),
	logtalk_load(demodb, [hook(rule_expansion(production)), report(warnings)]),
	logtalk_load(interpreterp, [report(warnings)]),
	logtalk_load(best_first, [report(warnings)]),
	logtalk_load(
		[
			dfs_interpreter,
			bfs_interpreter,
			iddfs_interpreter,
			bup_interpreter,
			a_star_interpreter
		],
		[
			hook(debug_expansion(production)),
			report(warnings)
		]
	),
	logtalk_load(shell, [hook(debug_expansion(production)), report(warnings)]),
	shell::welcome
)).

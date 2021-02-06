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


load_interpreters([]).
load_interpreters([I|Is]) :-
	functor(I, Name, _),
	logtalk_load(Name, [hook(debug_expansion(debug)), report(warnings), portability(warning)]),
	load_interpreters(Is).

:- initialization((
	Interpreters = [dfs_interpreter - rule_expansion(debug),
					bfs_interpreter - rule_expansion(debug),
					iddfs_interpreter(_Inc) - rule_expansion(debug),
					bup_interpreter - magic_expansion(debug),
					a_star_interpreter(_W) - heuristic_expansion(debug)],
	logtalk_load(
		[types(loader),
		 meta(loader),
		 heaps(loader),
		 queues(loader),
		 random(loader)],
		[report(off)]
	),
	logtalk_load(counter, [report(warnings), portability(warning)]),
	logtalk_load(magic, [report(warnings), portability(warning)]),
	logtalk_load(flatting, [report(warnings), portability(warning)]),
	logtalk_load(debug_expansion, [report(warnings), portability(warning)]),
	logtalk_load(rule_expansion, [report(warnings), portability(warning)]),
	logtalk_load(magic_expansion, [report(warnings), portability(warning)]),
	logtalk_load(shell_expansion, [report(warnings), portability(warning)]),
	logtalk_load(heuristic_expansion, [report(warnings), portability(warning)]),
	logtalk_load(benchmark_generators, [report(warnings), portability(warning)]),
	logtalk_load(databasep, [report(warnings), portability(warning)]),
	logtalk_load(demodb, [hook(rule_expansion(debug)), report(warnings), portability(warning)]),
	logtalk_load(interpreterp, [report(warnings), portability(warning)]),
	logtalk_load(best_first, [report(warnings), portability(warning)]),
	pairs::keys(Interpreters, Interpreters1),
	write(Interpreters1),
	load_interpreters(Interpreters1),
	logtalk_load(shell, [hook(debug_expansion(debug)), report(warnings), portability(warning)]),
	shell(Interpreters)::init)).

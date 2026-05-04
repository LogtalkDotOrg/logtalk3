%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Loader file for all installed packs
%  Last updated on April 3, 2022
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


% hack for running the Logtalk distribution tests in a CI/CD pipeline
% without generating code coverage stats for faster run times
%
% use with the logtalk_tester scripts -g option; for example:
%
% $ logtalk_tester -p swi -j 8 -o minimal -g "logtalk_load(library(skip_code_coverage))" -z

:- initialization(set_logtalk_flag(hook, user)).


:- multifile(term_expansion/2).
:- dynamic(term_expansion/2).

term_expansion((:- initialization(Goal)), (:- initialization(ExpandedGoal))) :-
	logtalk_load_context(basename, 'tester.lgt'),
	logtalk_load_context(directory, Directory),
	% some test sets require compilation in debug mode
	\+ sub_atom(Directory, _, _, _, assertions),
	\+ sub_atom(Directory, _, _, _, command_line_options),
	\+ sub_atom(Directory, _, _, _, ports_profiler),
	skip_code_coverage(Goal, ExpandedGoal).

skip_code_coverage((Goal1, Goal2), (ExpandedGoal1, ExpandedGoal2)) :-
	!,
	skip_code_coverage(Goal1, ExpandedGoal1),
	skip_code_coverage(Goal2, ExpandedGoal2).
skip_code_coverage(logtalk_load(File, Options), logtalk_load(File, ExpandedOptions)) :-
	!,
	skip_code_coverage_options(Options, ExpandedOptions).
skip_code_coverage(Goal, Goal).

skip_code_coverage_options([], []).
skip_code_coverage_options([debug(on)| Options], [optimize(on)| Options]) :-
	!.
skip_code_coverage_options([Option| Options], [Option| ExpandedOptions]) :-
	skip_code_coverage_options(Options, ExpandedOptions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This file is used by mutation testing subprocesses.
% It loads the test entities and runs the lgtunit tests that exercise them.

:- initialization((
	set_logtalk_flag(report, warnings),
	% Clause coverage collection requires trace events from debug-compiled entities.
	logtalk_load(test_entities, [debug(on), source_data(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(subprocess_tests, [hook(lgtunit), optimize(on)]),
	subprocess_tests::run
)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(basic_types(loader)),
	logtalk_load(options(loader)),
	logtalk_load(random(loader)),
	logtalk_load([
		ant_colony_problem_protocol,
		ant_colony
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load([
		'test_files/tsp',
		'test_files/tsp_progress',
		'test_files/tsp_stop',
		'test_files/atsp'
	], [
		optimize(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).

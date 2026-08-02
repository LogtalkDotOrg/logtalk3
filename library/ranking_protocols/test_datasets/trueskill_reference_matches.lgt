%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(trueskill_one_on_one_win,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, alpha_team, 0).
	team(round, beta_team, 1).
	team_member(round, alpha_team, alpha, 1.0).
	team_member(round, beta_team, beta, 1.0).

:- end_object.


:- object(trueskill_one_on_one_draw,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, alpha_team, 0).
	team(round, beta_team, 0).
	team_member(round, alpha_team, alpha, 1.0).
	team_member(round, beta_team, beta, 1.0).

:- end_object.


:- object(trueskill_free_for_all,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).
	match(round).
	team(round, alpha_team, 0).
	team(round, beta_team, 1).
	team(round, gamma_team, 2).
	team_member(round, alpha_team, alpha, 1.0).
	team_member(round, beta_team, beta, 1.0).
	team_member(round, gamma_team, gamma, 1.0).

:- end_object.

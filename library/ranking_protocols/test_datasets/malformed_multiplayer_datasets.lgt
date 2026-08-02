%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(multiplayer_duplicate_matches,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	match(round).
	team(round, first, 0).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, second, beta, 1.0).

:- end_object.


:- object(multiplayer_duplicate_teams,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, first, 0).
	team(round, first, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, first, beta, 1.0).

:- end_object.


:- object(multiplayer_invalid_rank,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, first, -1).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, second, beta, 1.0).

:- end_object.


:- object(multiplayer_one_team,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	match(round).
	team(round, only, 0).
	team_member(round, only, alpha, 1.0).

:- end_object.


:- object(multiplayer_empty_team,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	match(round).
	team(round, first, 0).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).

:- end_object.


:- object(multiplayer_unknown_item,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	match(round).
	team(round, first, 0).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, second, phantom, 1.0).

:- end_object.


:- object(multiplayer_invalid_weight,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, first, 0).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, second, beta, 1.5).

:- end_object.


:- object(multiplayer_duplicate_participant,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	match(round).
	team(round, first, 0).
	team(round, second, 1).
	team_member(round, first, alpha, 1.0).
	team_member(round, second, alpha, 1.0).
	team_member(round, second, beta, 1.0).

:- end_object.

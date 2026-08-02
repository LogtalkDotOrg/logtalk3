%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(multiplayer_matches,
	implements(multiplayer_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).
	item(delta).
	item(epsilon).

	match(opening).
	match(final).

	team(opening, red, 0).
	team(opening, blue, 1).
	team(final, alpha_team, 1).
	team(final, gamma_team, 1).

	team_member(opening, red, alpha, 1.0).
	team_member(opening, red, beta, 0.5).
	team_member(opening, blue, gamma, 1.0).
	team_member(opening, blue, delta, 1.0).
	team_member(final, alpha_team, alpha, 1.0).
	team_member(final, gamma_team, gamma, 1.0).

:- end_object.

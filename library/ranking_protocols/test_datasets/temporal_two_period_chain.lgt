%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(temporal_two_period_chain,
	implements(temporal_pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).

	period(round1).
	period(round2).

	game(round1, alpha, beta, 1.0).
	game(round1, beta, gamma, 1.0).
	game(round2, alpha, gamma, 1.0).

:- end_object.

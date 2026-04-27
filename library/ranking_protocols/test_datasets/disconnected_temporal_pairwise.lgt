%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(disconnected_temporal_pairwise,
	implements(temporal_pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).
	item(delta).

	period(round1).

	game(round1, alpha, beta, 1.0).
	game(round1, gamma, delta, 1.0).

:- end_object.
